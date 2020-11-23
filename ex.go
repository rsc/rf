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
	"strconv"
	"strings"

	"rsc.io/rf/edit"
	"rsc.io/rf/refactor"
)

type example struct {
	old ast.Node
	new ast.Node
}

func cmdEx(snap *refactor.Snapshot, text string) {
	code, err := parseEx(snap, text)
	if err != nil {
		snap.ErrorAt(token.NoPos, "ex: %v", err)
		return
	}

	if _, err := checkEx(snap, code); err != nil {
		snap.ErrorAt(token.NoPos, "ex: %v", err)
		return
	}
	return
}

func cutGo(text, sep string) (before, after string, ok bool, err error) {
	// TODO
	before, after, ok = cut(text, sep)
	return before, after, ok, nil
}

func parseEx(snap *refactor.Snapshot, text string) (code string, err error) {
	fset := token.NewFileSet()
	var buf bytes.Buffer
	if true { // TODO: single vs multiple targets
		fmt.Fprintf(&buf, "package %s\n", snap.Target().Types.Name())
	} else {
		fmt.Fprintf(&buf, "package ex\n")
	}
	text = strings.TrimSpace(text)
	importOK := true
	for text != "" {
		stmt, rest, _, err := cutGo(text, ";")
		if err != nil {
			return "", err
		}
		text = rest
		stmt = strings.TrimSpace(stmt)
		if stmt == "" {
			continue
		}
		switch kw := strings.Fields(stmt)[0]; kw {
		case "package", "type", "func", "const":
			return "", fmt.Errorf("%s declaration not allowed", kw)

		case "defer", "for", "go", "if", "return", "select", "switch":
			return "", fmt.Errorf("%s statement not allowed", kw)

		case "import":
			file, err := parser.ParseFile(fset, "ex.go", "package p;"+stmt, 0)
			if err != nil {
				return "", fmt.Errorf("parsing %s: %v", stmt, err)
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
				return "", fmt.Errorf("import %q not available", pkg)
			}
			if !importOK {
				return "", fmt.Errorf("parsing %s: import too late", stmt)
			}
			fmt.Fprintf(&buf, "%s\n", stmt)

		case "var":
			if _, err := parser.ParseExpr("func() {" + stmt + "}"); err != nil {
				return "", fmt.Errorf("parsing %s: %v", stmt, err)
			}
			if importOK {
				fmt.Fprintf(&buf, "func _() {\n")
				importOK = false
			}
			fmt.Fprintf(&buf, "%s\n", stmt)

		default:
			// Must be rewrite x -> y.
			// No possible error from cutGo
			// because we already processed it once.
			before, after, ok, _ := cutGo(stmt, "->")
			if !ok {
				return "", fmt.Errorf("parsing: %s: missing -> in rewrite", stmt)
			}
			// TODO: parse stmt / parse expr
			if importOK {
				fmt.Fprintf(&buf, "func _() {\n")
				importOK = false
			}
			fmt.Fprintf(&buf, "{\n{%s}\n{%s}\n}\n", before, after)
		}
	}
	if importOK {
		return "", fmt.Errorf("no example rewrites")
	}
	fmt.Fprintf(&buf, "}\n")
	return buf.String(), nil
}

func checkEx(snap *refactor.Snapshot, code string) ([]example, error) {
	codePos := token.Pos(snap.Fset().Base())
	f, err := parser.ParseFile(snap.Fset(), "ex.go", code, 0)
	if err != nil {
		println(code)
		return nil, fmt.Errorf("internal error: %v", err)
	}

	var errors int
	conf := &types.Config{
		Error: func(err error) {
			if strings.HasSuffix(err.Error(), " is not used") {
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

	var info *types.Info
	var typesPkg *types.Package
	if true { // TODO single vs double
		p := snap.Target()
		info = p.TypesInfo
		typesPkg = p.Types
	} else {
		info = &types.Info{
			Types:      make(map[ast.Expr]types.TypeAndValue),
			Defs:       make(map[*ast.Ident]types.Object),
			Uses:       make(map[*ast.Ident]types.Object),
			Selections: make(map[*ast.SelectorExpr]*types.Selection),
		}
	}

	if typesPkg == nil {
		typesPkg = types.NewPackage("ex", "ex")
	}
	check := types.NewChecker(conf, snap.Fset(), typesPkg, info)
	err = check.Files([]*ast.File{f})
	_ = err // already handled in conf.Error
	if errors != 0 {
		return nil, fmt.Errorf("errors in example:\n%s", code)
	}

	body := f.Decls[len(f.Decls)-1].(*ast.FuncDecl).Body.List

	for _, stmt := range body {
		stmt, ok := stmt.(*ast.BlockStmt)
		if !ok { // var decl
			continue
		}
		var pattern, subst ast.Node
		pattern = stmt.List[0].(*ast.BlockStmt).List[0]
		if x, ok := pattern.(*ast.ExprStmt); ok {
			pattern = x.X
		}
		subst = stmt.List[1].(*ast.BlockStmt).List[0]
		if x, ok := subst.(*ast.ExprStmt); ok {
			subst = x.X
		}
		applyEx(snap, code, codePos, typesPkg, info, pattern, subst)
	}
	return nil, nil
}

func avoidOf(snap *refactor.Snapshot, info *types.Info, subst ast.Node) map[ast.Node]bool {
	avoid := make(map[ast.Node]bool)
	refactor.Walk(subst, func(stack []ast.Node) {
		id, ok := stack[0].(*ast.Ident)
		if !ok {
			return
		}
		if obj := info.Uses[id]; obj != nil {
			stack := snap.SyntaxAt(obj.Pos())
			for i := 0; i < len(stack); i++ {
				switch n := stack[i].(type) {
				case *ast.FuncDecl, *ast.GenDecl:
					avoid[n] = true
				}
			}
		}
	})
	return avoid
}

func applyEx(snap *refactor.Snapshot, code string, codePos token.Pos, typesPkg *types.Package, info *types.Info, pattern, subst ast.Node) {
	m := &matcher{
		fset:    snap.Fset(),
		wildOK:  true,
		wildPos: codePos,
		pkg:     typesPkg,
		info:    info,
		env:     make(map[string]ast.Expr),
	}

	var avoid map[ast.Node]bool

	snap.ForEachTargetFile(func(target *refactor.Package, file *ast.File) {
		refactor.Walk(file, func(stack []ast.Node) {
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

			if m.match(pattern, stack[0]) {
				// Do not apply substitution in its own definition.
				if avoid == nil {
					avoid = avoidOf(snap, info, subst)
				}
				for _, n := range stack {
					if avoid[n] {
						return
					}
				}

				// Do not substitute a function call on LHS of assignment.
				// TODO: Generalize.
				if as, ok := stack[1].(*ast.AssignStmt); ok {
					for _, l := range as.Lhs {
						if l == stack[0] {
							if _, isCall := subst.(*ast.CallExpr); isCall {
								return
							}
						}
					}
				}

				matchPos := stack[0].Pos()
				// Substitute pattern variable values from match into substitution text.
				// Because these values are coming from the same source location
				// as they will eventually be placed into, import references and the
				// like are all OK and don't need updating.
				buf := edit.NewBuffer([]byte(code[subst.Pos()-codePos : subst.End()-codePos]))
				refactor.Walk(subst, func(stack []ast.Node) {
					id, ok := stack[0].(*ast.Ident)
					if !ok {
						return
					}

					// Pattern variable -> captured subexpression.
					if xobj, ok := m.wildcardObj(id); ok {
						replx := m.env[xobj.Name()]
						// TODO: captured subexpression may need import fixes?
						repl := string(snap.Text(replx.Pos(), replx.End()))
						if needParen(replx, stack) {
							repl = "(" + repl + ")"
						}
						buf.Replace(int(id.Pos()-subst.Pos()), int(id.End()-subst.Pos()), repl)
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
							buf.Replace(int(id.Pos()-subst.Pos()), int(id.Pos()-subst.Pos()), typesPkg.Name()+".")
						}
					}
				})

				// Now substitute completed substitution text into actual program.
				substX := subst
				if id, ok := substX.(*ast.Ident); ok {
					if xobj, ok := m.wildcardObj(id); ok {
						substX = m.env[xobj.Name()]
					}
				}
				substText := buf.String()
				if needParen(substX, stack) {
					substText = "(" + substText + ")"
				}

				// Do not replace text that isn't changing.
				// Helps with x.M() -> x.R() applied to z.M().M().M().
				old := string(snap.Text(stack[0].Pos(), stack[0].End()))
				i := 0
				for i < len(old) && i < len(substText) && old[i] == substText[i] {
					i++
				}
				snap.ReplaceAt(stack[0].Pos()+token.Pos(i), stack[0].End(), substText[i:])
			}
		})
	})
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
		*ast.CompositeLit:
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
