// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"go/types"
	"path"
	"strconv"
	"strings"

	"rsc.io/rf/refactor"
)

// exArgs holds the result of parsing an ex command invocation.
type exArgs struct {
	snap       *refactor.Snapshot
	patternPkg *refactor.Package
	targets    []*refactor.Package
	avoids     []types.Object
	stricts    map[types.Object]bool
	implicits  map[types.Object]bool
	code       string
	codePos    token.Pos
	rewrites   []example
	typeAssert bool // command is typeassert not ex
}

type example struct {
	old ast.Node
	new ast.Node
}

func cmdEx(snap *refactor.Snapshot, text string) error {
	ex, err := parseEx(snap, text, false)
	if err != nil {
		return err
	}
	if err := ex.check(); err != nil {
		return err
	}
	ex.run()
	return nil
}

func cmdTypeAssert(snap *refactor.Snapshot, text string) error {
	ex, err := parseEx(snap, text, true)
	if err != nil {
		return err
	}
	if err := ex.check(); err != nil {
		return err
	}
	ex.runTypeAssert()
	return nil
}

func cutGo(text, sep string) (before, after string, ok bool, err error) {
	before, after, ok = cut(text, sep)
	return before, after, ok, nil
}

func parseEx(snap *refactor.Snapshot, text string, isTypeAssert bool) (*exArgs, error) {
	fset := token.NewFileSet()
	text = strings.TrimSpace(text)
	if text == "" {
		return nil, newErrUsage("missing block")
	}
	i := strings.Index(text, "{")
	if i < 0 || text[len(text)-1] != '}' {
		return nil, newErrUsage("malformed block - missing { }")
	}
	before := strings.TrimSpace(text[:i])

	patternPkg := snap.Target()
	targets := []*refactor.Package{patternPkg}
	if before != "" {
		targets = nil
		items, _ := snap.EvalList(before)
	Items:
		for _, item := range items {
			if item.Kind == refactor.ItemNotFound {
				return nil, newErrPrecondition("%s not found", item.Name)
			}
			if item.Kind != refactor.ItemDir {
				return nil, newErrPrecondition("%s is not a package", item.Name)
			}
			pkgPath := item.Name
			if pkgPath == "." || strings.HasPrefix(pkgPath, "./") || strings.HasPrefix(pkgPath, "../") {
				pkgPath = path.Join(snap.Target().PkgPath, pkgPath)
			}
			for _, p := range snap.Packages() {
				if p.PkgPath == pkgPath {
					targets = append(targets, p)
					continue Items
				}
			}
			return nil, newErrPrecondition("cannot find package %s", pkgPath)
		}
	}
	text = strings.TrimSpace(text[i+1 : len(text)-1])
	if text == "" {
		return nil, newErrUsage("empty block")
	}

	var buf bytes.Buffer
	fmt.Fprintf(&buf, "package %s\n", patternPkg.Name)
	importOK := true
	body := func() {
		if importOK {
			fmt.Fprintf(&buf, "func _() { type any interface{}\n")
			for _, pragma := range []string{"avoid", "strict", "implicit"} {
				fmt.Fprintf(&buf, "var __%s__ func(...interface{})\n_ = __%s__\n", pragma, pragma)
			}
			importOK = false
		}
	}
	for text != "" {
		stmt, rest, _ := cutAny(text, ";\n") // TODO
		text = rest
		stmt = strings.TrimSpace(stmt)
		if stmt == "" {
			continue
		}
		switch kw := strings.Fields(stmt)[0]; kw {
		case "package", "func", "const":
			return nil, newErrUsage("%s declaration not allowed", kw)

		case "defer", "for", "go", "if", "return", "select", "switch":
			return nil, newErrUsage("%s statement not allowed", kw)

		case "import":
			file, err := parser.ParseFile(fset, "ex.go", "package p;"+stmt, 0)
			if err != nil {
				return nil, newErrUsage("parsing %s: %v", stmt, err)
			}
			imp := file.Imports[0]
			pkg := importPath(imp)
			have := false
			for _, p := range snap.Packages() {
				if p.Types.Path() == pkg {
					have = true
				}
			}
			if !have {
				return nil, newErrUsage("import %q not available", pkg)
			}
			if !importOK {
				return nil, newErrUsage("parsing %s: import too late", stmt)
			}
			fmt.Fprintf(&buf, "%s\n", stmt)

		case "type", "var":
			if _, err := parser.ParseExpr("func() {" + stmt + "}"); err != nil {
				return nil, newErrUsage("parsing %s: %v", stmt, err)
			}
			body()
			fmt.Fprintf(&buf, "%s\n", stmt)

		case "avoid", "strict", "implicit":
			stmt = strings.TrimSpace(strings.TrimPrefix(stmt, kw))
			if stmt == "" {
				return nil, newErrUsage("missing arguments for %s", kw)
			}
			if _, err := parser.ParseExpr("f(" + stmt + ")"); err != nil {
				return nil, newErrUsage("parsing %s: %v", stmt, err)
			}
			body()
			fmt.Fprintf(&buf, "__%s__(%s)\n", kw, stmt)

		default:
			// Must be rewrite x -> y.
			// No possible error from cutGo
			// because we already processed it once.
			before, after, ok, _ := cutGo(stmt, "->")
			if !ok {
				return nil, newErrUsage("parsing: %s: missing -> in rewrite", stmt)
			}
			before = strings.TrimSpace(before)
			after = strings.TrimSpace(after)
			if before == "" {
				return nil, newErrUsage("missing pattern in example: %s", stmt)
			}
			if after == "" {
				return nil, newErrUsage("missing substitution in example: %s", stmt)
			}

			body()
			if _, err := parser.ParseExpr("func() {" + before + "}"); err != nil {
				before = "type _ " + before
				if _, err := parser.ParseExpr("func() { " + before + "}"); err != nil {
					return nil, newErrUsage("parsing %s: %v", stmt, err)
				}
			}
			fmt.Fprintf(&buf, "{\n")
			fmt.Fprintf(&buf, "{\n%s /* -> %s */\n}\n", before, after)
			if after != "!" {
				fn, err := parser.ParseExpr("func() {" + after + "}")
				if err != nil && !isTypeAssert {
					after = "type _ " + after
					if _, err := parser.ParseExpr("func() { " + after + "}"); err != nil {
						return nil, newErrUsage("parsing %s: %v", stmt, err)
					}
				}
				if isTypeAssert {
					foundAssert := false
					s := fn.(*ast.FuncLit).Body.List[0]
					if s, ok := s.(*ast.ExprStmt); ok {
						if ta, ok := s.X.(*ast.TypeAssertExpr); ok {
							if _, ok := ta.X.(*ast.Ident); ok {
								foundAssert = true
							}
						}
					}
					if !foundAssert {
						return nil, newErrUsage("parsing %s: arrow must be followed by name.(T)", stmt)
					}
				}
				fmt.Fprintf(&buf, "{\n/* %s -> */ %s\n}\n", before, after)
			}
			fmt.Fprintf(&buf, "}\n")
		}
	}
	if importOK {
		return nil, newErrUsage("no example rewrites")
	}
	fmt.Fprintf(&buf, "}\n")

	ex := &exArgs{
		snap:       snap,
		patternPkg: patternPkg,
		targets:    targets,
		code:       buf.String(),
		typeAssert: isTypeAssert,
	}
	return ex, nil
}

func (ex *exArgs) check() error {
	snap := ex.snap
	codePos := token.Pos(snap.Fset().Base())
	var codeLines []string
	f, err := parser.ParseFile(snap.Fset(), "ex.go", ex.code, 0)
	if err != nil {
		return fmt.Errorf("internal error: %v\ncode:\n%s", err, ex.code)
	}

	var errs refactor.ErrorList
	conf := &types.Config{
		Error: func(err error) {
			if strings.HasSuffix(err.Error(), " is not used") {
				return
			}
			if strings.HasSuffix(err.Error(), " declared but not used") ||
				strings.HasSuffix(err.Error(), " declared and not used") {
				return
			}
			if strings.HasSuffix(err.Error(), " (type) is not an expression") {
				return
			}
			if err, ok := err.(types.Error); ok {
				if codeLines == nil {
					codeLines = strings.Split(ex.code, "\n")
				}
				posn := snap.Fset().Position(err.Pos)
				if posn.Filename == "ex.go" && posn.Line >= 1 && posn.Line < len(codeLines) {
					errs.Add(fmt.Errorf("%s\n%s", err.Msg, codeLines[posn.Line-1]))
					return
				}
			}
			errs.Add(err)
		},
		Importer: importerFunc(func(pkg string) (*types.Package, error) {
			for _, p := range snap.Packages() {
				if p.Types.Path() == pkg {
					return p.Types, nil
				}
			}
			return nil, fmt.Errorf("unknown import %q", pkg)
		}),
	}

	check := types.NewChecker(conf, snap.Fset(), ex.patternPkg.Types, ex.patternPkg.TypesInfo)
	err = check.Files([]*ast.File{f})
	_ = err // already handled in conf.Error
	if err := errs.Err(); err != nil {
		return newErrPrecondition("errors in example:\n%s", err.Error())
	}

	var avoids []types.Object
	stricts := map[types.Object]bool{}
	implicits := map[types.Object]bool{}

	body := f.Decls[len(f.Decls)-1].(*ast.FuncDecl).Body.List

	info := ex.patternPkg.TypesInfo
	var rewrites []example
	for _, stmt := range body {
		if stmt, ok := stmt.(*ast.ExprStmt); ok {
			if pragma, ok := stmt.X.(*ast.CallExpr); ok {
				kw := strings.Trim(pragma.Fun.(*ast.Ident).Name, "_")

				for _, n := range pragma.Args {
					// New thing to avoid.
					var obj types.Object
					switch n := n.(type) {
					default:
						return newErrUsage("cannot %s %T", kw, n)
					case *ast.Ident:
						obj = info.Uses[n]
					case *ast.SelectorExpr:
						obj = info.Uses[n.Sel]
					}
					if obj == nil {
						return newErrPrecondition("cannot find %s %v", kw, astString(snap.Fset(), n))
					}
					switch kw {
					case "avoid":
						avoids = append(avoids, obj)
					case "strict":
						if obj, ok := obj.(*types.Var); ok && obj.Pos() >= codePos {
							stricts[obj] = true
							break
						}
						return newErrPrecondition("%s: %v is not a pattern variable", kw, obj)
					case "implicit":
						// TODO(mdempsky): Support multi-valued expressions?
						if obj, ok := obj.(*types.Var); ok && obj.Pos() >= codePos {
							if typ, ok := obj.Type().(*types.Signature); ok && typ.Params().Len() == 1 && !typ.Variadic() {
								implicits[obj] = true
								break
							}
						}
						return newErrPrecondition("%s: %v is not a single-parameter function-typed variable", kw, obj)
					default:
						panic("unreachable")
					}
				}
			}
		}
		stmt, ok := stmt.(*ast.BlockStmt)
		if !ok { // var decl
			continue
		}
		var pattern, subst ast.Node
		pattern = stmt.List[0].(*ast.BlockStmt).List[0]
		switch x := pattern.(type) {
		case *ast.ExprStmt:
			pattern = x.X
		case *ast.DeclStmt:
			pattern = x.Decl.(*ast.GenDecl).Specs[0].(*ast.TypeSpec).Type
		}
		if len(stmt.List) >= 2 {
			subst = stmt.List[1].(*ast.BlockStmt).List[0]
			switch x := subst.(type) {
			case *ast.ExprStmt:
				subst = x.X
			case *ast.DeclStmt:
				subst = x.Decl.(*ast.GenDecl).Specs[0].(*ast.TypeSpec).Type
			}
		}
		if ex.typeAssert {
			x, ok := pattern.(*ast.BinaryExpr)
			if !ok || x.Op != token.EQL {
				return newErrUsage("invalid typeassert condition (must be ==): %v", astString(snap.Fset(), pattern))
			}
		}
		rewrites = append(rewrites, example{pattern, subst})
	}

	ex.avoids = avoids
	ex.stricts = stricts
	ex.implicits = implicits
	ex.codePos = codePos
	ex.rewrites = rewrites
	return nil
}

func avoidOf(snap *refactor.Snapshot, avoids []types.Object, substInfo *types.Info, subst ast.Node) map[ast.Node]bool {
	avoid := make(map[ast.Node]bool)
	avoidObj := func(obj types.Object) {
		stack := snap.SyntaxAt(obj.Pos())
		for i := 0; i < len(stack); i++ {
			switch n := stack[i].(type) {
			case *ast.FuncDecl, *ast.GenDecl:
				avoid[n] = true
			}
		}
	}
	for _, obj := range avoids {
		avoidObj(obj)
	}
	if subst != nil {
		refactor.Walk(subst, func(stack []ast.Node) {
			id, ok := stack[0].(*ast.Ident)
			if !ok {
				return
			}
			// Definitions from the universe and built-ins don't have a
			// position, but there's also no chance of walking into them so
			// there's nothing to avoid.
			if obj := substInfo.Uses[id]; obj != nil && obj.Parent() != types.Universe && !isBuiltin(obj) {
				avoidObj(obj)
			}
		})
	}
	return avoid
}

func isBuiltin(obj types.Object) bool {
	_, isBuiltin := obj.(*types.Builtin)
	return isBuiltin
}

func (ex *exArgs) run() {
	m := &matcher{
		snap:    ex.snap,
		code:    ex.code,
		codePos: ex.codePos,
		fset:    ex.snap.Fset(),
		wildOK:  true, // TODO(rsc): remove
		wildPos: ex.codePos,
		pkgX:    ex.patternPkg.Types,
		infoX:   ex.patternPkg.TypesInfo,
		env:     make(map[types.Object]envBind),
		envT:    make(map[string]types.Type),
		stricts: ex.stricts,
	}

	var avoid map[ast.Node]bool

	for _, target := range ex.targets {
		m.target = target
		m.pkgY = target.Types
		m.infoY = target.TypesInfo
		for _, file := range target.Files {
			if file.Syntax == nil {
				continue
			}

			// Post-order traversal lets us rewrite children before parents,
			// which means that the parent rewrite is wrapping the child node,
			// its appending edits apply *after* any appending edits in the child.
			// For example consider:
			//	x -> fx()
			//	y = x -> sety(x)
			// The first rewrite appends "()", and the second ")".
			// We want to end up with sety(fx()).
			// Postorder places the inner rewrites appends before the outer one.
			// This is not quite correct in general, but it works well.
			refactor.WalkPost(file.Syntax, func(stack []ast.Node) {
				// Do not match against the bare selector within a qualified identifier.
				if len(stack) >= 2 {
					if sel, ok := stack[1].(*ast.SelectorExpr); ok && sel.Sel == stack[0] {
						if x, ok := sel.X.(*ast.Ident); ok {
							if _, ok := m.infoY.Uses[x].(*types.PkgName); ok {
								return
							}
						}
					}
				}

				if _, ok := stack[0].(*ast.ParenExpr); ok {
					// Matcher is blind to parens.
					// Let the inside match, and avoid a spurious match here too.
					return
				}

				for _, example := range ex.rewrites {
					pattern, subst := example.old, example.new

					m.reset()

					if call, ok := pattern.(*ast.CallExpr); ok {
						if ident, ok := call.Fun.(*ast.Ident); ok {
							if lval := ex.patternPkg.TypesInfo.Uses[ident]; ex.implicits[lval] {
								typ := assigneeType(stack, target.TypesInfo)
								if typ == nil || !m.identical(typ, lval.Type().(*types.Signature).Params().At(0).Type()) {
									continue
								}

								pattern = call.Args[0]
							}
						}
					}

					if !m.match(pattern, stack[0]) {
						continue
					}

					// Matched a "!" pattern.
					// Return to avoid trying further patterns.
					if subst == nil {
						return
					}

					if !contextAppropriate(subst, ex.patternPkg.TypesInfo, stack, target.TypesInfo) {
						continue
					}

					// Do not apply substitution in its own definition.
					// TODO(rsc): This cache is wrong: it should build a different avoid map for
					// each different subst, not cache the first for all rewrites.
					// It's also probably wrong that an avoid applies to rewrites
					// above it in the input. Restructing this would probably help
					// but we'd run into the post-order traversal bug again too.
					// More thought is needed.
					if avoid == nil {
						avoid = avoidOf(ex.snap, ex.avoids, ex.patternPkg.TypesInfo, subst)
					}
					for _, n := range stack {
						if avoid[n] {
							return
						}
					}

					substText, target := m.applySubst(subst, stack)
					if lit := m.compositeLitNeedsParen(subst, stack); lit != nil {
						substText = "(" + substText
						ex.snap.InsertAt(lit.End(), ")")
					}

					replaceMinimal(ex.snap, target.Pos(), target.End(), substText)

					// We're done with this AST node.
					// Don't try any further patterns.
					return
				}
			})
		}
	}
}

func (m *matcher) applySubst(subst ast.Node, matchContext []ast.Node) (string, ast.Node) {
	// Substitute pattern variable values from match into substitution text.
	// Because these values are coming from the same source location
	// as they will eventually be placed into, import references and the
	// like are all OK and don't need updating.
	matchPos := matchContext[0].Pos()
	buf := refactor.NewBufferAt(m.snap, subst.Pos(), []byte(m.code[subst.Pos()-m.codePos:subst.End()-m.codePos]))
	refactor.Walk(subst, func(stack []ast.Node) {
		id, ok := stack[0].(*ast.Ident)
		if !ok {
			return
		}

		// Pattern variable -> captured subexpression.
		if xobj, ok := m.wildcardObj(id); ok {
			var repl string
			switch xobj := xobj.(type) {
			case *types.Var:
				b := m.env[xobj]
				replx := b.matchExpr

				var outer ast.Node
				if len(stack) >= 2 {
					outer = stack[1]
				} else if len(matchContext) >= 2 {
					outer = matchContext[1]
				}

				// Apply implicit op except in selector expressions.
				op := b.implicitOp
				if op != 0 && outer != nil {
					if sel, ok := outer.(*ast.SelectorExpr); ok && sel.X == id {
						// Selector will apply the implicit op for us.
						op = 0
					}
					if addr, ok := outer.(*ast.UnaryExpr); ok && op == '*' && addr.Op == token.AND {
						// Delete the outer &.
						buf.Delete(outer.Pos(), outer.Pos()+1)
						op = 0
					}
					if _, ok := outer.(*ast.StarExpr); ok && op == '&' {
						// Delete the outer *.
						buf.Delete(outer.Pos(), outer.Pos()+1)
						op = 0
					}
				}
				if op == '*' {
					if addr, ok := replx.(*ast.UnaryExpr); ok && addr.Op == token.AND {
						// Delete the inner &.
						replx = addr.X
						op = 0
					}
					if px, ok := replx.(*ast.ParenExpr); ok {
						if addr, ok := px.X.(*ast.UnaryExpr); ok && addr.Op == token.AND {
							// Delete the inner &.
							replx = addr.X
							op = 0
						}
					}
				}
				if op == '&' {
					if addr, ok := replx.(*ast.UnaryExpr); ok && addr.Op == token.AND {
						// Delete the inner *.
						replx = addr.X
						op = 0
					}
					if px, ok := replx.(*ast.ParenExpr); ok {
						if addr, ok := px.X.(*ast.UnaryExpr); ok && addr.Op == token.AND {
							// Delete the inner *.
							replx = addr.X
							op = 0
						}
					}
				}

				// If substituting an explicit * or & into a selector, drop the * or & when possible.
				if sel, ok := outer.(*ast.SelectorExpr); ok && sel.X == id && op == 0 {
					if addr, ok := replx.(*ast.UnaryExpr); ok && addr.Op == token.AND && !isPointer(typeOf(m.infoY, addr.X)) {
						// Selector will insert implicit & to convert non-pointer to pointer.
						replx = addr.X
					} else if star, ok := replx.(*ast.StarExpr); ok && !isPointer(typeOf(m.infoY, star)) {
						// Selector will insert implicit * to convert pointer to non-pointer.
						replx = star.X
					}
				}

				// The captured subexpression does not need import fixes,
				// because it is coming from the original source file (where it is staying).
				// It can be substituted directly, possibly with parens and implicit * or &.
				repl = string(m.snap.Text(replx.Pos(), replx.End()))
				if op != 0 {
					repl = string(b.implicitOp) + repl
					replx = &ast.UnaryExpr{X: replx} // only for precedence for needParen
				}
				if needParen(replx, stack) || len(stack) == 1 && needParen(replx, matchContext) {
					repl = "(" + repl + ")"
				}
			case *types.TypeName:
				typ := m.envT[xobj.Name()]
				repl = types.TypeString(typ, func(pkg *types.Package) string {
					if pkg == m.target.Types {
						return ""
					}
					// TODO(mdempsky): Handle missing and renamed imports.
					return pkg.Name()
				})
				if needParenType(typ, stack) {
					repl = "(" + repl + ")"
				}
			default:
				panic("unreachable")
			}
			buf.Replace(id.Pos(), id.End(), repl)
			return
		}

		// If this ID is p.ID where p is a package,
		// make sure we have the import available,
		// or if we are inserting into package p, remove the p.
		if len(stack) >= 2 {
			if sel, ok := stack[1].(*ast.SelectorExpr); ok && sel.X == stack[0] {
				if xid, ok := sel.X.(*ast.Ident); ok {
					if pid, ok := m.infoX.Uses[xid].(*types.PkgName); ok {
						if pid.Imported() == m.target.Types {
							sobj := m.infoX.Uses[sel.Sel]
							xobj := m.snap.LookupAt(sel.Sel.Name, matchPos)
							if xobj != sobj {
								m.snap.ErrorAt(matchPos, "%s is shadowed in replacement - %v", sel.Sel.Name, xobj)
							}
							buf.Delete(sel.Pos(), sel.Sel.Pos())
						} else {
							name := m.snap.NeedImport(matchPos, "", pid.Imported())
							buf.Replace(sel.Pos(), sel.Sel.Pos(), name+".")
						}
					}
				}
				return
			}
		}

		// Is this ID referring to a global in the original package? If so:
		// - Is the original package the target package?
		//   Then make sure the global isn't shadowed.
		// - Otherwise, make sure the target has an import, and qualify the identifier.
		obj := m.infoX.Uses[id]
		if m.pkgX != nil && obj.Parent() == m.pkgX.Scope() {
			if m.pkgX == m.target.Types {
				if xobj := m.snap.LookupAt(id.Name, matchPos); xobj != obj {
					m.snap.ErrorAt(matchPos, "%s is shadowed in replacement - %v", id.Name, xobj)
				}
			} else {
				name := m.snap.NeedImport(matchPos, m.pkgX.Name(), m.pkgX)
				buf.Insert(id.Pos(), name+".")
			}
		}
	})

	if needParen(subst, matchContext) {
		return "(" + buf.String() + ")", matchContext[0]
	}

	return buf.String(), matchContext[0]
}

func isPointer(t types.Type) bool {
	_, ok := t.(*types.Pointer)
	return ok
}

// needParen reports whether replacing stack[0] with newX requires parens around newX.
func needParen(newX ast.Node, stack []ast.Node) bool {
	if len(stack) == 1 {
		return false
	}
	inner, outer := stack[0], stack[1]
	if _, ok := outer.(ast.Expr); !ok {
		// Context is not an expression; no chance of an expression breaking apart.
		return false
	}

	var prec int
	switch newX := newX.(type) {
	default:
		panic(fmt.Sprintf("needParen inner %T", newX))
	case *ast.SelectorExpr,
		*ast.TypeAssertExpr,
		*ast.CallExpr,
		*ast.IndexExpr,
		*ast.SliceExpr,
		*ast.ParenExpr,
		*ast.Ident,
		*ast.BasicLit,
		*ast.CompositeLit,
		*ast.ArrayType,
		*ast.MapType:
		return false // nothing can tear these apart
	case *ast.ChanType:
		if newX.Dir != ast.RECV {
			return false
		}
	case *ast.BinaryExpr:
		prec = newX.Op.Precedence()
	case *ast.StarExpr, *ast.UnaryExpr:
		prec = token.UnaryPrec
	}

	switch outer := outer.(type) {
	default:
		panic(fmt.Sprintf("needParen outer %T", outer))
	case *ast.BinaryExpr:
		return prec < outer.Op.Precedence()
	case *ast.StarExpr, *ast.UnaryExpr:
		return prec < token.UnaryPrec
	case *ast.SelectorExpr:
		return prec < token.HighestPrec
	case *ast.TypeAssertExpr:
		if inner == outer.X {
			return prec < token.HighestPrec
		}
		return false
	case *ast.KeyValueExpr, *ast.ParenExpr:
		return false // arguments are safe
	case *ast.CallExpr:
		if inner == outer.Fun {
			return prec < token.HighestPrec
		}
		return false
	case *ast.IndexExpr:
		if inner == outer.X {
			return prec < token.HighestPrec
		}
		return false // arguments are safe
	case *ast.SliceExpr:
		if inner == outer.X {
			return prec < token.HighestPrec
		}
		return false // arguments are safe
	case *ast.CompositeLit:
		return false
	case *ast.ArrayType, *ast.MapType:
		return false
	case *ast.ChanType:
		if outer.Dir != ast.SEND|ast.RECV {
			return false
		}
		_, need := newX.(*ast.ChanType)
		return need
	}
}

func needParenType(newT types.Type, stack []ast.Node) bool {
	// Substituting "<-chan T" into "chan _" needs to become "chan
	// (<-chan T)", otherwise it will be parsed as "chan<- (chan T)".
	if ch1, ok := newT.(*types.Chan); ok && ch1.Dir() == types.RecvOnly {
		if ch2, ok := stack[1].(*ast.ChanType); ok && ch2.Dir == ast.SEND|ast.RECV {
			return true
		}
	}

	return false
}

// compositeLitNeedsParen reports whether we're substituting an
// identifier into the type of a composite literal expression, which
// appears in an ambiguous context (i.e., between an "if", "for", or
// "switch" keyword and the correspsonding open brace token).
func (m *matcher) compositeLitNeedsParen(subst ast.Node, stack []ast.Node) *ast.CompositeLit {
	if _, ok := subst.(*ast.Ident); !ok {
		return nil
	}

	lit, ok := stack[1].(*ast.CompositeLit)
	if !ok || lit.Type != stack[0] {
		return nil
	}

	for i := 2; i < len(stack); i++ {
		inner := stack[i-1]
		switch outer := stack[i].(type) {
		case *ast.IfStmt, *ast.ForStmt, *ast.SwitchStmt, *ast.TypeSwitchStmt:
			return lit

		// enclosed in parentheses
		case *ast.ParenExpr:
			return nil
		case *ast.CallExpr:
			for _, arg := range outer.Args {
				if arg == inner {
					return nil
				}
			}
		case *ast.TypeAssertExpr:
			if outer.Type == inner {
				return nil
			}

		// enclosed in square brackets
		case *ast.IndexExpr:
			if outer.Index == inner {
				return nil
			}
		case *ast.SliceExpr:
			if outer.Low == inner || outer.High == inner || outer.Max == inner {
				return nil
			}
		case *ast.ArrayType:
			if outer.Len == inner {
				return nil
			}
		case *ast.MapType:
			if outer.Key == inner {
				return nil
			}

		// enclosed in curly braces
		case *ast.BlockStmt, *ast.StructType, *ast.InterfaceType:
			return nil
		case *ast.CompositeLit:
			for _, elt := range outer.Elts {
				if elt == inner {
					return nil
				}
			}
		}
	}

	return nil
}

// importPath returns the unquoted import path of s,
// or "" if the path is not properly quoted.
func importPath(s *ast.ImportSpec) string {
	t, err := strconv.Unquote(s.Path.Value)
	if err != nil {
		return ""
	}
	return t
}

type importerFunc func(string) (*types.Package, error)

func (f importerFunc) Import(s string) (*types.Package, error) { return f(s) }

func replaceMinimal(snap *refactor.Snapshot, pos, end token.Pos, repl string) {
	text := snap.Text(pos, end)
	posX := 0
	posY := 0
	for _, r := range commonRanges(string(text), repl) {
		snap.ReplaceAt(pos+token.Pos(posX), pos+token.Pos(r.posX), repl[posY:r.posY])
		posX = r.posX + r.n
		posY = r.posY + r.n
	}
	snap.ReplaceAt(pos+token.Pos(posX), end, repl[posY:])
}

type rangePair struct{ posX, posY, n int }

func commonRanges(x, y string) []rangePair {
	// t[i,j] = length of longest common substring of x[i:], y[j:]
	// t[i,len(y)] = t[len(x),j] = 0
	// t[i,j] = max {
	//	t[i+1,j]
	//	t[i,j+1]
	//	t[i+1,j+1] + 1 only if x[i] == y[j]
	// }
	t := make([][]int, len(x)+1)
	data := make([]int, (len(x)+1)*(len(y)+1))
	for i := range t {
		t[i], data = data[:len(y)+1], data[len(y)+1:]
	}

	for i := len(x) - 1; i >= 0; i-- {
		for j := len(y) - 1; j >= 0; j-- {
			m := t[i+1][j]
			if m < t[i][j+1] {
				m = t[i][j+1]
			}
			if x[i] == y[j] {
				if m < t[i+1][j+1]+1 {
					m = t[i+1][j+1] + 1
				}
			}
			t[i][j] = m
		}
	}

	/*
		fmt.Println(x)
		fmt.Println(y)
		for _, row := range t {
			fmt.Println(row)
		}
	*/

	i := 0
	j := 0
	var pairs []rangePair
	for i < len(x) && j < len(y) {
		switch m := t[i][j]; {
		case m == t[i+1][j+1]+1 && x[i] == y[j]:
			// Start a new range.
			posX := i
			posY := j
			for i < len(x) && j < len(y) && t[i][j] == t[i+1][j+1]+1 && x[i] == y[j] {
				i++
				j++
			}
			pairs = append(pairs, rangePair{posX, posY, i - posX})

		case m == t[i+1][j]:
			i++

		case m == t[i][j+1]:
			j++

		default:
			panic("inconsistent")
		}
	}
	return pairs
}

// assigneeType returns the type of the variable that stack[0] is
// being assigned to. Assignment here includes the assignment implied
// by passing arguments to a function call, return results,
// initializing composite literals, and map indexing.
//
// If the variable at the top of the stack is not being assigned, then
// assigneeType returns nil.
func assigneeType(stack []ast.Node, info *types.Info) types.Type {
	if len(stack) < 2 {
		return nil
	}

	val := stack[0]
	switch parent := stack[1].(type) {
	case *ast.AssignStmt:
		if len(parent.Lhs) != len(parent.Rhs) {
			break
		}
		for i, rhs := range parent.Rhs {
			if rhs == val {
				return info.TypeOf(parent.Lhs[i])
			}
		}

	case *ast.ValueSpec:
		if len(parent.Names) != len(parent.Values) {
			break
		}
		for i, rhs := range parent.Values {
			if rhs == val {
				return info.TypeOf(parent.Names[i])
			}
		}

	case *ast.CallExpr:
		if parent.Fun == val {
			break
		}

		tv, ok := info.Types[parent.Fun]
		if !ok {
			panic(fmt.Sprintf("missing type info for %v", parent.Fun))
		}

		// Type conversion.
		if tv.IsType() {
			return tv.Type
		}

		// go/types doesn't assign an inferred signature for all builtins (e.g., len).
		if tv.IsBuiltin() && tv.Type == types.Typ[types.Invalid] {
			return nil
		}

		// Function call.
		sig := tv.Type.Underlying().(*types.Signature)
		params := sig.Params()
		last := params.Len() - 1
		for i, arg := range parent.Args {
			if arg == val {
				if sig.Variadic() && parent.Ellipsis == token.NoPos && i >= last {
					return params.At(last).Type().(*types.Slice).Elem()
				}
				return params.At(i).Type()
			}
		}

	case *ast.ReturnStmt:
		var curfn ast.Expr
	Outer:
		for _, outer := range stack[2:] {
			switch outer := outer.(type) {
			case *ast.FuncDecl:
				curfn = outer.Name
				break Outer
			case *ast.FuncLit:
				curfn = outer
				break Outer
			}
		}

		sig := info.TypeOf(curfn).(*types.Signature)
		for i, result := range parent.Results {
			if result == val {
				return sig.Results().At(i).Type()
			}
		}

	case *ast.CompositeLit:
		if parent.Type == val {
			break
		}
		switch typ := info.TypeOf(parent).Underlying().(type) {
		case *types.Array:
			return typ.Elem()
		case *types.Slice:
			return typ.Elem()
		case *types.Struct:
			for i, elt := range parent.Elts {
				if elt == val {
					return typ.Field(i).Type()
				}
			}

		}

	case *ast.KeyValueExpr:
		outer := stack[2].(*ast.CompositeLit)
		switch typ := info.TypeOf(outer).Underlying().(type) {
		case *types.Array:
			if parent.Value == val {
				return typ.Elem()
			}
		case *types.Slice:
			if parent.Value == val {
				return typ.Elem()
			}
		case *types.Map:
			if parent.Key == val {
				return typ.Key()
			}
			if parent.Value == val {
				return typ.Elem()
			}
		case *types.Struct:
			if parent.Value == val {
				return info.Uses[parent.Key.(*ast.Ident)].(*types.Var).Type()
			}
		}

	case *ast.IndexExpr:
		if typ, ok := info.TypeOf(parent.X).Underlying().(*types.Map); ok && parent.Index == val {
			return typ.Key()
		}
	}

	return nil
}

func contextAppropriate(subst ast.Node, substInfo *types.Info, stack []ast.Node, stackInfo *types.Info) bool {
	if len(stack) < 2 {
		return true // is anything ever appropriate here?
	}

	// TODO: More cases.

	expr, ok := subst.(ast.Expr)
	if !ok {
		return true
	}
	tv := substInfo.Types[expr]

	switch parent := stack[1].(type) {
	case *ast.AssignStmt:
		// Don't substitution non-assignable expression on LHS
		// of assignment.
		for _, l := range parent.Lhs {
			if l == stack[0] {
				if !tv.Assignable() {
					return false
				}
				break
			}
		}

		// Don't substitute RHS of assignment if it isn't
		// appropriate for LHS.
		if len(parent.Rhs) == 1 && parent.Rhs[0] == stack[0] {
			want := len(parent.Lhs)
			if tup, ok := tv.Type.(*types.Tuple); ok {
				if tup.Len() != want {
					// multi-value function result
					// doesn't match LHS arity
					return false
				}
			} else if want == 2 && tv.HasOk() {
				// ok
			} else if want > 1 {
				// single-value expression assigned to
				// multiple variables
				return false
			}
		}

	case *ast.UnaryExpr:
		// Don't substitution non-addressable expression as
		// operand of unary &.
		if parent.Op == token.AND && !tv.Addressable() {
			return false
		}
	}

	return true
}

func (ex *exArgs) runTypeAssert() {
	snap := ex.snap
	m := &matcher{
		snap:    snap,
		code:    ex.code,
		codePos: ex.codePos,
		fset:    snap.Fset(),
		wildOK:  true,
		wildPos: ex.codePos,
		pkgX:    ex.patternPkg.Types,
		infoX:   ex.patternPkg.TypesInfo,
		env:     make(map[types.Object]envBind),
		envT:    make(map[string]types.Type),
	}

	// TODO(rsc): This is almost as wrong as the other avoidOf call.
	// See comment above.
	avoid := avoidOf(snap, ex.avoids, ex.patternPkg.TypesInfo, nil)

	done := make(map[types.Object][]span)

	for _, target := range ex.targets {
		m.target = target
		m.pkgY = target.Types
		m.infoY = target.TypesInfo
		for _, file := range target.Files {
			if file.Syntax == nil {
				continue
			}

			refactor.Walk(file.Syntax, func(stack []ast.Node) {
				for _, n := range stack {
					if avoid[n] {
						return
					}
				}

				switch stack[0].(type) {
				case *ast.IfStmt:
					typeAssertIf(m, stack, ex.rewrites, done)
				case *ast.CaseClause:
					if _, ok := stack[2].(*ast.SwitchStmt); ok {
						typeAssertCase(m, stack, ex.rewrites, done)
					}
				}
			})
		}
	}
}

type span struct {
	pos token.Pos
	end token.Pos
}

func typeAssertIf(m *matcher, stack []ast.Node, typeAsserts []example, done map[types.Object][]span) {
	ifs := stack[0].(*ast.IfStmt)
	t, f := matchCond(m, ifs.Cond, typeAsserts)

	if t != nil {
		maybeAssert(m, t, ifs.Body.List, stack, done)
	}
	if f != nil {
		if ifs.Else != nil {
			maybeAssert(m, f, ifs.Else.(*ast.BlockStmt).List, stack, done)
		} else {
			// Special case: maybe insert the assertion after the if statement,
			// if the "then" body diverts out of the scope where the assertion
			// would be inserted.
			if id, obj, ok := rematch(m, f); ok && alwaysReturns(ifs.Body.List) {
				_ = id // Could use these to refine the check instead of using alwaysReturns.
				_ = obj
				// Find enclosing block statement.
				for i := 1; i < len(stack); i++ {
					var list []ast.Stmt
					switch stmt := stack[i].(type) {
					case *ast.IfStmt, *ast.LabeledStmt:
						// Skipping past "IfStmt-Else IfStmt" and "LabeledStmt: IfStmt".
						continue
					case *ast.BlockStmt:
						list = stmt.List
					case *ast.CaseClause:
						list = stmt.Body
					default:
						panic(fmt.Sprintf("unexpected %T", stmt))
					}
					for j := 0; j < len(list); j++ {
						if list[j] == stack[i-1] {
							maybeAssert(m, f, list[j+1:], stack[i:], done)
							break
						}
					}
					return
				}
			}
		}
	}
}

func typeAssertCase(m *matcher, stack []ast.Node, typeAsserts []example, done map[types.Object][]span) {
	cas := stack[0].(*ast.CaseClause)
	sw := stack[2].(*ast.SwitchStmt)

	var t *typeAssertMatch
	for _, val := range cas.List {
		t1, _ := matchCondCmp(m, sw.Tag, token.EQL, val, typeAsserts)
		if t1 == nil {
			return
		}
		if t == nil {
			t = t1
		} else {
			t = joinTypeAssert(m, t, t1)
			if t == nil {
				return
			}
		}
	}
	if t != nil {
		maybeAssert(m, t, cas.Body, stack, done)
	}
}

type typeAssertMatch struct {
	condX, condY ast.Expr
	ex           example
}

func matchCond(m *matcher, cond ast.Expr, typeAsserts []example) (t, f *typeAssertMatch) {
	if p, ok := cond.(*ast.ParenExpr); ok {
		return matchCond(m, p.X, typeAsserts)
	}
	if u, ok := cond.(*ast.UnaryExpr); ok && u.Op == token.NOT {
		t, f := matchCond(m, u.X, typeAsserts)
		return f, t
	}
	c, ok := cond.(*ast.BinaryExpr)
	if !ok {
		return nil, nil
	}
	switch c.Op {
	case token.EQL, token.NEQ:
		return matchCondCmp(m, c.X, c.Op, c.Y, typeAsserts)

	case token.LAND:
		tX, fX := matchCond(m, c.X, typeAsserts)
		tY, fY := matchCond(m, c.Y, typeAsserts)
		// X && Y true => both X and Y are true, so both tX and tY apply. pick first (mostly likely only one match).
		if tX != nil {
			t = tX
		} else {
			t = tY
		}
		// X && Y false => one of X and Y is false but we don't know which.
		// Only hope is that they lead to the same conclusion.
		if fX != nil && fY != nil {
			f = joinTypeAssert(m, fX, fY)
		}
		return

	case token.LOR:
		tX, fX := matchCond(m, c.X, typeAsserts)
		tY, fY := matchCond(m, c.Y, typeAsserts)
		// X || Y true => one of X or Y is true, but we don't know which.
		// Only hope is that they lead to the same conclusion.
		if tX != nil && tY != nil {
			t = joinTypeAssert(m, tX, tY)
		}
		// X || Y false => both X and Y are false, so both fX and fY apply. pick first.
		if fX != nil {
			f = fX
		} else {
			f = fY
		}
		return
	}

	return nil, nil
}

func matchCondCmp(m *matcher, x ast.Expr, op token.Token, y ast.Expr, typeAsserts []example) (t, f *typeAssertMatch) {
	for _, ex := range typeAsserts {
		p := ex.old.(*ast.BinaryExpr)
		m.reset()
		if m.match(p.X, x) && m.match(p.Y, y) {
			if op == token.EQL {
				t = &typeAssertMatch{x, y, ex} // cond true => rewrite applies
			} else {
				f = &typeAssertMatch{x, y, ex} // cond (x != y) false => rewrite applies
			}
			return
		}
	}
	return nil, nil
}

func joinTypeAssert(m *matcher, t1, t2 *typeAssertMatch) *typeAssertMatch {
	m.reset()
	p1 := t1.ex.old.(*ast.BinaryExpr)
	p2 := t2.ex.old.(*ast.BinaryExpr)
	if m.match(p1.X, t1.condX) && m.match(p2.X, t2.condX) && m.match(p1.Y, t1.condY) && m.match(p2.Y, t2.condY) {
		pkgY, infoY := m.pkgY, m.infoY
		m.pkgY, m.infoY = m.pkgX, m.infoX
		ok := m.match(t1.ex.new, t2.ex.new)
		m.pkgY, m.infoY = pkgY, infoY
		if ok {
			// The match environments are compatible,
			// and the conclusions are the same.
			return t1
		}
	}
	return nil
}

// alwaysReturns reports whether a particular sequence of statements
// in a body is guaranteed to return from the function.
func alwaysReturns(list []ast.Stmt) bool {
	if len(list) == 0 {
		return false
	}

	// Any branch may end up elsewhere.
	// TODO: We could refine this a bit.
	for _, stmt := range list {
		bad := false
		refactor.Walk(stmt, func(stack []ast.Node) {
			if _, ok := stack[0].(*ast.BranchStmt); ok {
				bad = true
			}
		})
		if bad {
			return false
		}
	}

	// Otherwise the last statement is what matters.
	stmt := list[len(list)-1]
	for {
		if l, ok := stmt.(*ast.LabeledStmt); ok {
			stmt = l.Stmt
			continue
		}
		break
	}

	switch stmt := stmt.(type) {
	case *ast.ReturnStmt:
		return true
	case *ast.IfStmt:
		return stmt.Else != nil && alwaysReturns(stmt.Body.List) && alwaysReturns(stmt.Else.(*ast.BlockStmt).List)
	case *ast.SwitchStmt:
		def := false
		for _, cas := range stmt.Body.List {
			cas := cas.(*ast.CaseClause)
			if !alwaysReturns(cas.Body) {
				return false
			}
			if cas.List == nil {
				def = true
			}
		}
		return def
	}
	return false
}

func rematch(m *matcher, t *typeAssertMatch) (id *ast.Ident, obj types.Object, allOK bool) {
	m.reset()
	p1 := t.ex.old.(*ast.BinaryExpr)
	if !m.match(p1.X, t.condX) || !m.match(p1.Y, t.condY) {
		m.snap.ErrorAt(t.condY.Pos(), "internal error: lost match")
	}

	xid := t.ex.new.(*ast.TypeAssertExpr).X.(*ast.Ident)
	xobj := m.infoX.Uses[xid]
	if xobj == nil {
		m.snap.ErrorAt(t.condY.Pos(), "internal error: lost obj for %s", xid.Name)
		return
	}
	x, ok := m.env[xobj]
	if !ok {
		m.snap.ErrorAt(t.condY.Pos(), "internal error: lost match for %s", xid.Name)
		return
	}
	id, ok = x.matchExpr.(*ast.Ident)
	if !ok {
		// Cannot introduce := for non-identifier. Ignore.
		return
	}
	obj = m.infoY.Uses[id]
	if obj == nil {
		m.snap.ErrorAt(t.condY.Pos(), "internal error: lost match for %s", id.Name)
		return
	}
	return id, obj, true
}

func maybeAssert(m *matcher, t *typeAssertMatch, list []ast.Stmt, stack []ast.Node, done map[types.Object][]span) {
	id, obj, ok := rematch(m, t)
	if !ok || len(list) == 0 {
		return
	}
	if !types.IsInterface(m.infoY.TypeOf(id)) {
		return
	}
	matchPos := list[0].Pos()
	if m.snap.ScopeAt(matchPos-1) == obj.Parent() {
		// Cannot reassert in same scope as declaration.
		return
	}
	for _, s := range done[obj] {
		if s.pos <= matchPos && matchPos < s.end {
			// Already reasserted in higher syntax.
			return
		}
	}

	found := false
	assigned := false
	badAssign := false
	for _, stmt := range list {
		refactor.Walk(stmt, func(stack []ast.Node) {
			id, ok := stack[0].(*ast.Ident)
			if !ok {
				return
			}
			if m.infoY.Uses[id] != obj {
				return
			}
			if as, ok := stack[1].(*ast.AssignStmt); ok {
				for i, l := range as.Lhs {
					if l == id {
						assigned = true
						if !types.AssignableTo(m.infoY.TypeOf(as.Rhs[i]), m.infoX.TypeOf(t.ex.new.(ast.Expr))) {
							badAssign = true
							return
						}
						return
					}
				}
			}
			if _, ok := stack[1].(*ast.TypeAssertExpr); ok {
				// Cannot use concrete type in type assertion.
				badAssign = true
				return
			}
			found = true
		})
	}
	if !found {
		// m.snap.InsertAt(list[0].Pos(), "/* not found */")
		return
	}
	if badAssign {
		// m.snap.InsertAt(list[0].Pos(), "/* bad assign */")
		return
	}
	if assigned && !alwaysReturns(list) {
		// TODO: scan forward in list looking to see if it falls through.
		// if not, all good.

		// TODO: otherwise, look in stack beyond block for use anywhere
		// if so, bad.
		// keep track of labels seen and gotos seen.
		// if any label is unseen but the goto is seen,
		// label is above the assignment, so bad.
		// if closing a for loop, bad
		// if final part terminates, OK.
		// m.snap.InsertAt(list[0].Pos(), "/* assigned */")
		return
	}

	done[obj] = append(done[obj], span{matchPos, list[len(list)-1].End()})
	substText, _ := m.applySubst(t.ex.new, []ast.Node{list[0]})
	m.snap.InsertAt(matchPos, id.Name+" := "+substText+";")
}
