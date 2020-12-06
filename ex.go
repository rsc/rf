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
	targets []*refactor.Package
	code    string
}

type example struct {
	old ast.Node
	new ast.Node
}

func cmdEx(snap *refactor.Snapshot, text string) {
	args, err := parseEx(snap, text)
	if err != nil {
		snap.ErrorAt(token.NoPos, "ex: %v", err)
		return
	}
	if args == nil {
		// Trust that parseEx printed an error.
		return
	}

	if _, err := checkEx(snap, args); err != nil {
		snap.ErrorAt(token.NoPos, "ex: %v", err)
		return
	}
	return
}

func cutGo(text, sep string) (before, after string, ok bool, err error) {
	before, after, ok = cut(text, sep)
	return before, after, ok, nil
}

func parseEx(snap *refactor.Snapshot, text string) (args *exArgs, err error) {
	fset := token.NewFileSet()
	text = strings.TrimSpace(text)
	if text == "" {
		snap.ErrorAt(token.NoPos, "ex: missing block")
		return
	}
	i := strings.Index(text, "{")
	if i < 0 || text[len(text)-1] != '}' {
		snap.ErrorAt(token.NoPos, "ex: malformed block - missing { }")
		return
	}
	before := strings.TrimSpace(text[:i])
	targets := []*refactor.Package{snap.Target()}
	if before != "" {
		targets = nil
		items, _ := snap.EvalList(before)
		ok := true
	Items:
		for _, item := range items {
			if item.Kind != refactor.ItemDir {
				snap.ErrorAt(token.NoPos, "ex: %s is not a package", item.Name)
				ok = false
				continue
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
			snap.ErrorAt(token.NoPos, "ex: cannot find package %s", pkgPath)
			ok = false
		}
		if !ok {
			return
		}
	}
	text = strings.TrimSpace(text[i+1 : len(text)-1])
	if text == "" {
		snap.ErrorAt(token.NoPos, "ex: empty block")
		return
	}

	var buf bytes.Buffer
	if true || len(targets) == 1 {
		fmt.Fprintf(&buf, "package %s\n", targets[0].Types.Name())
	} else {
		fmt.Fprintf(&buf, "package ex\n")
	}
	importOK := true
	body := func() {
		if importOK {
			fmt.Fprintf(&buf, "func _() { type any interface{}\nvar __avoid__, __strict__ func(...interface{})\n_, _ = __avoid__, __strict__\n")
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
			return nil, fmt.Errorf("%s declaration not allowed", kw)

		case "defer", "for", "go", "if", "return", "select", "switch":
			return nil, fmt.Errorf("%s statement not allowed", kw)

		case "import":
			file, err := parser.ParseFile(fset, "ex.go", "package p;"+stmt, 0)
			if err != nil {
				return nil, fmt.Errorf("parsing %s: %v", stmt, err)
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
				return nil, fmt.Errorf("import %q not available", pkg)
			}
			if !importOK {
				return nil, fmt.Errorf("parsing %s: import too late", stmt)
			}
			fmt.Fprintf(&buf, "%s\n", stmt)

		case "type", "var":
			if _, err := parser.ParseExpr("func() {" + stmt + "}"); err != nil {
				return nil, fmt.Errorf("parsing %s: %v", stmt, err)
			}
			body()
			fmt.Fprintf(&buf, "%s\n", stmt)

		case "avoid", "strict":
			stmt = strings.TrimSpace(strings.TrimPrefix(stmt, kw))
			if stmt == "" {
				return nil, fmt.Errorf("missing arguments for %s", kw)
			}
			if _, err := parser.ParseExpr("f(" + stmt + ")"); err != nil {
				return nil, fmt.Errorf("parsing %s: %v", stmt, err)
			}
			body()
			fmt.Fprintf(&buf, "__%s__(%s)\n", kw, stmt)

		default:
			// Must be rewrite x -> y.
			// No possible error from cutGo
			// because we already processed it once.
			before, after, ok, _ := cutGo(stmt, "->")
			if !ok {
				return nil, fmt.Errorf("parsing: %s: missing -> in rewrite", stmt)
			}
			before = strings.TrimSpace(before)
			after = strings.TrimSpace(after)
			if before == "" {
				return nil, fmt.Errorf("missing pattern in example: %s", stmt)
			}
			if after == "" {
				return nil, fmt.Errorf("missing substitution in example: %s", stmt)
			}

			body()
			if _, err := parser.ParseExpr("func() {" + before + "}"); err != nil {
				before = "type _ " + before
				if _, err := parser.ParseExpr("func() { " + before + "}"); err != nil {
					return nil, fmt.Errorf("parsing %s: %v", stmt, err)
				}
			}
			fmt.Fprintf(&buf, "{\n{%s}\n", before)
			if after != "!" {
				if _, err := parser.ParseExpr("func() {" + after + "}"); err != nil {
					after = "type _ " + after
					if _, err := parser.ParseExpr("func() { " + after + "}"); err != nil {
						return nil, fmt.Errorf("parsing %s: %v", stmt, err)
					}
				}
				fmt.Fprintf(&buf, "{%s}\n", after)
			}
			fmt.Fprintf(&buf, "}\n")
		}
	}
	if importOK {
		return nil, fmt.Errorf("no example rewrites")
	}
	fmt.Fprintf(&buf, "}\n")

	return &exArgs{targets: targets, code: buf.String()}, nil
}

func checkEx(snap *refactor.Snapshot, args *exArgs) ([]example, error) {
	codePos := token.Pos(snap.Fset().Base())
	f, err := parser.ParseFile(snap.Fset(), "ex.go", args.code, 0)
	if err != nil {
		return nil, fmt.Errorf("internal error: %v\ncode:\n%s", err, args.code)
	}

	var errors int
	conf := &types.Config{
		Error: func(err error) {
			if strings.HasSuffix(err.Error(), " is not used") {
				return
			}
			if strings.HasSuffix(err.Error(), " (type) is not an expression") {
				return
			}
			errors++
			snap.ErrorAt(token.NoPos, "%v", err)
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

	var typesPkg *types.Package
	var info *types.Info
	if true || len(args.targets) == 1 {
		p := args.targets[0]
		typesPkg = p.Types
		info = p.TypesInfo
	} else {
		typesPkg = types.NewPackage("ex", "ex")
		info = &types.Info{
			Types:      make(map[ast.Expr]types.TypeAndValue),
			Defs:       make(map[*ast.Ident]types.Object),
			Uses:       make(map[*ast.Ident]types.Object),
			Selections: make(map[*ast.SelectorExpr]*types.Selection),
			Scopes:     make(map[ast.Node]*types.Scope),
			Implicits:  make(map[ast.Node]types.Object),
		}
	}

	check := types.NewChecker(conf, snap.Fset(), typesPkg, info)
	err = check.Files([]*ast.File{f})
	_ = err // already handled in conf.Error
	if errors != 0 {
		return nil, fmt.Errorf("errors in example:\n%s", args.code)
	}

	var avoids []types.Object
	stricts := map[types.Object]bool{}

	body := f.Decls[len(f.Decls)-1].(*ast.FuncDecl).Body.List

	var examples []example
	for _, stmt := range body {
		if stmt, ok := stmt.(*ast.ExprStmt); ok {
			if pragma, ok := stmt.X.(*ast.CallExpr); ok {
				kw := strings.Trim(pragma.Fun.(*ast.Ident).Name, "_")

				for _, n := range pragma.Args {
					// New thing to avoid.
					var obj types.Object
					switch n := n.(type) {
					default:
						return nil, fmt.Errorf("cannot %s %T", kw, n)

					case *ast.Ident:
						obj = info.Uses[n]

					case *ast.SelectorExpr:
						sel := info.Selections[n]
						if sel != nil {
							obj = sel.Obj()
						}
					}
					if obj == nil {
						return nil, fmt.Errorf("cannot find %s %v", kw, astString(snap.Fset(), n))
					}
					switch kw {
					case "avoid":
						avoids = append(avoids, obj)
					case "strict":
						if obj, ok := obj.(*types.Var); ok && obj.Pos() >= codePos {
							stricts[obj] = true
						} else {
							return nil, fmt.Errorf("strict: %v is not a pattern variable", obj)
						}
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
		examples = append(examples, example{pattern, subst})
	}
	applyEx(snap, args.targets, avoids, stricts, args.code, codePos, typesPkg, info, examples)
	return nil, nil
}

func avoidOf(snap *refactor.Snapshot, avoids []types.Object, info *types.Info, subst ast.Node) map[ast.Node]bool {
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
	refactor.Walk(subst, func(stack []ast.Node) {
		id, ok := stack[0].(*ast.Ident)
		if !ok {
			return
		}
		if obj := info.Uses[id]; obj != nil && obj.Parent() != types.Universe {
			avoidObj(obj)
		}
	})
	return avoid
}

func applyEx(snap *refactor.Snapshot, targets []*refactor.Package, avoids []types.Object, stricts map[types.Object]bool, code string, codePos token.Pos, typesPkg *types.Package, info *types.Info, examples []example) {
	m := &matcher{
		fset:    snap.Fset(),
		wildOK:  true,
		wildPos: codePos,
		pkgX:    typesPkg,
		infoX:   info,
		env:     make(map[string]ast.Expr),
		envT:    make(map[string]types.Type),
		stricts: stricts,
	}

	var avoid map[ast.Node]bool

	for _, target := range targets {
		m.pkgY = target.Types
		m.infoY = target.TypesInfo
		for _, file := range target.Files {
			if file.Syntax == nil {
				continue
			}
			refactor.Walk(file.Syntax, func(stack []ast.Node) {
				// Do not match against the bare selector within a qualified identifier.
				if len(stack) >= 2 {
					if sel, ok := stack[1].(*ast.SelectorExpr); ok && sel.Sel == stack[0] {
						if x, ok := sel.X.(*ast.Ident); ok {
							if _, ok := info.Uses[x].(*types.PkgName); ok {
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

				for _, example := range examples {
					pattern, subst := example.old, example.new

					if !m.match(pattern, stack[0]) {
						continue
					}

					// Matched a "!" pattern.
					// Return to avoid trying further patterns.
					if subst == nil {
						return
					}

					// Do not apply substitution in its own definition.
					if avoid == nil {
						avoid = avoidOf(snap, avoids, info, subst)
					}
					for _, n := range stack {
						if avoid[n] {
							return
						}
					}

					// Do not substitute a function call
					// on LHS of assignment, or as argument to unary &.
					// TODO: Generalize.
					if _, isCall := subst.(*ast.CallExpr); isCall {
						if as, ok := stack[1].(*ast.AssignStmt); ok {
							for _, l := range as.Lhs {
								if l == stack[0] {
									return
								}
							}
						}
						if addr, ok := stack[1].(*ast.UnaryExpr); ok && false {
							if addr.Op == token.AND && addr.X == stack[0] {
								return
							}
						}
					}

					matchPos := stack[0].Pos()
					// Substitute pattern variable values from match into substitution text.
					// Because these values are coming from the same source location
					// as they will eventually be placed into, import references and the
					// like are all OK and don't need updating.
					buf := refactor.NewBufferAt(snap, subst.Pos(), []byte(code[subst.Pos()-codePos:subst.End()-codePos]))
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
								replx := m.env[xobj.Name()]
								// TODO: captured subexpression may need import fixes?
								repl = string(snap.Text(replx.Pos(), replx.End()))
								if needParen(replx, stack) {
									repl = "(" + repl + ")"
								}
							case *types.TypeName:
								typ := m.envT[xobj.Name()]
								repl = types.TypeString(typ, func(pkg *types.Package) string {
									if pkg == typesPkg {
										return ""
									}
									// TODO(mdempsky): Handle missing and renamed imports.
									return pkg.Name()
								})
							default:
								panic("unreachable")
							}
							buf.Replace(id.Pos(), id.End(), repl)
							return
						}

						// If this ID is p.ID where p is a package,
						// make sure we have the import available,
						// or if this is package p, remove the p.
						if len(stack) >= 2 {
							if sel, ok := stack[1].(*ast.SelectorExpr); ok {
								if xid, ok := sel.X.(*ast.Ident); ok {
									if pid, ok := info.Uses[xid].(*types.PkgName); ok {
										snap.NeedImport(matchPos, xid.Name, pid.Imported())
									}
								}
								return
							}
						}

						// Is this ID referring to a global in the typesPkg? If so:
						// - Is the typesPkg the target package? Then make sure the global isn't shadowed.
						// - Otherwise, make sure the target has an import, and qualify the identifier.
						obj := info.Uses[id]
						if obj == nil {
							panic("NO USES")
						}
						if obj != nil && typesPkg != nil && obj.Parent() == typesPkg.Scope() {
							if typesPkg == target.Types {
								if xobj := snap.LookupAt(id.Name, matchPos); xobj != obj {
									snap.ErrorAt(matchPos, "%s is shadowed in replacement - %v", id.Name, xobj)
								}
							} else {
								snap.NeedImport(matchPos, typesPkg.Name(), typesPkg)
								buf.Insert(id.Pos(), typesPkg.Name()+".")
							}
						}
					})

					// Now substitute completed substitution text into actual program.
					substX := subst
					if id, ok := substX.(*ast.Ident); ok {
						if xobj, ok := m.wildcardObj(id); ok {
							switch xobj := xobj.(type) {
							case *types.Var:
								substX = m.env[xobj.Name()]
							case *types.TypeName:
								panic("TODO: do we need to do anything here?")
							default:
								panic("unreachable")
							}
						}
					}
					substText := buf.String()
					if needParen(substX, stack) {
						substText = "(" + substText + ")"
					}
					replaceMinimal(snap, stack[0].Pos(), stack[0].End(), substText)

					// We're done with this AST node.
					// Don't try any further patterns.
					return
				}
			})
		}
	}
	return
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
	case *ast.SelectorExpr, *ast.TypeAssertExpr:
		return prec < token.HighestPrec
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
	}
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
