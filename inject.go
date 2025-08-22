// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/types"
	"strings"

	"rsc.io/rf/refactor"
)

// origin returns the origin for the given [types.Object]. Normally, this is
// the object itself, but for instantiated methods or fields it is the
// corresponding object on the generic type.
func origin(obj types.Object) types.Object {
	switch t := obj.(type) {
	case *types.Func:
		return t.Origin()
	case *types.Var:
		return t.Origin()
	default:
		return obj
	}
}

func cmdInject(snap *refactor.Snapshot, args string) error {
	items, _ := snap.EvalList(args)
	if len(items) < 2 {
		return newErrUsage("inject Var Func...")
	}

	for _, item := range items {
		if item.Kind == refactor.ItemNotFound {
			return newErrPrecondition("%s not found", item.Name)
		}
	}

	targetObj := items[0].Obj
	qv := snap.QualNameOf(targetObj)
	if qv == (refactor.QualName{}) {
		if items[0].Obj == nil {
			panic("no obj for " + items[0].Name)
		}
		panic("lost item: " + items[0].Name)
	}
	targetName := strings.ToLower(qv.Name)

	deps := snap.DepsGraph(refactor.SymRefs)
	rdeps := deps.Reverse()

	usesVar := make(map[types.Object]bool)
	var add func(refactor.QualName)
	add = func(q refactor.QualName) {
		obj := q.Object()
		if obj == nil {
			panic("lost obj")
		}
		if usesVar[obj] {
			return
		}
		usesVar[obj] = true
		for p, m := range rdeps.G[q.Pkg][q.Name] {
			for name := range m {
				q1 := refactor.QualName{Pkg: p, Name: name}
				obj1 := q1.Object()
				if _, ok := obj1.(*types.Func); !ok {
					continue
				}
				add(q1)
			}
		}
	}
	add(qv)
	delete(usesVar, targetObj)

	converting := make(map[types.Object]string)
	add = func(q refactor.QualName) {
		obj := q.Object()
		if obj == nil {
			panic("lost obj: " + q.String())
		}
		if converting[obj] != "" || !usesVar[obj] {
			return
		}
		converting[obj] = "."
		for p, m := range deps.G[q.Pkg][q.Name] {
			for name := range m {
				q1 := refactor.QualName{Pkg: p, Name: name}
				if !usesVar[q1.Object()] {
					continue
				}
				add(q1)
			}
		}
	}

	roots := make(map[types.Object]bool)
	for _, item := range items[1:] {
		roots[item.Obj] = true
		q := snap.QualNameOf(item.Obj)
		add(q)
	}
	for obj := range roots {
		delete(converting, obj)
	}

	snap.ForEachFile(func(pkg *refactor.Package, file *ast.File) {
		refactor.Walk(file, func(stack []ast.Node) {
			id, ok := stack[0].(*ast.Ident)
			if !ok || len(stack) < 2 {
				return
			}
			obj := origin(pkg.TypesInfo.Uses[id])
			if converting[obj] == "" {
				return
			}
			if call, ok := stack[1].(*ast.CallExpr); ok && call.Fun == id {
				// ok
				return
			} else if sel, ok := stack[1].(*ast.SelectorExpr); ok && sel.Sel == id {
				if call, ok := stack[2].(*ast.CallExpr); ok && call.Fun == stack[1] {
					// ok
					return
				}
			}
			//	snap.ErrorAt(id.Pos(), "%v not called - skipping conversion", obj)
			delete(converting, obj)
		})
	})

	snap.ForEachFile(func(pkg *refactor.Package, file *ast.File) {
		for _, decl := range file.Decls {
			if fn, ok := decl.(*ast.FuncDecl); ok {
				obj := pkg.TypesInfo.Defs[fn.Name]
				if obj == nil {
					fmt.Printf("MISSING %v\n", fn.Name)
				}
				if converting[obj] == "" {
					continue
				}
				// Determine a name for the parameter.
				uses := make(map[string]bool)
				refactor.Walk(fn.Body, func(stack []ast.Node) {
					if id, ok := stack[0].(*ast.Ident); ok {
						if sel, ok := stack[1].(*ast.SelectorExpr); ok && sel.Sel == id {
							return
						}
						if kv, ok := stack[1].(*ast.KeyValueExpr); ok && kv.Key == id {
							return
						}
						uses[id.Name] = true
					}
				})
				dst := fn.Type.Params.Opening + 1
				name := targetName
				for uses[name] {
					name += "_"
				}
				converting[obj] = name
				var b bytes.Buffer
				fmt.Fprintf(&b, "%s ", name)
				printType(&b, snap, targetObj.Pos(), dst, targetObj.Type())
				if len(fn.Type.Params.List) > 0 {
					b.WriteString(", ")
				}
				snap.InsertAt(dst, b.String())
			}
		}
	})

	snap.ForEachFile(func(pkg *refactor.Package, file *ast.File) {
		refactor.Walk(file, func(stack []ast.Node) {
			id, ok := stack[0].(*ast.Ident)
			if !ok || len(stack) < 2 {
				return
			}
			obj := origin(pkg.TypesInfo.Uses[id])
			if converting[obj] == "" && obj != targetObj {
				return
			}
			// If inside a function body, stack[len-1] = *ast.File, stack[len-2] = *ast.FuncDecl.
			name := ""
			if fn, ok := stack[len(stack)-2].(*ast.FuncDecl); ok {
				fnobj := pkg.TypesInfo.Defs[fn.Name]
				name = converting[fnobj]
			}
			if obj == targetObj {
				if name != "" {
					pos := ast.Node(id)
					if sel, ok := stack[1].(*ast.SelectorExpr); ok && sel.Sel == id {
						pos = sel
					}
					snap.ReplaceNode(pos, name)
				}
				return
			}
			fixName := func() {
				if name == "" {
					name = items[0].Name
					outer := items[0].Outermost()
					if outer.Kind == refactor.ItemPkg {
						if pkg.Types == outer.Obj.(*types.PkgName).Imported() {
							// Don't add an import if we are already in that package
							name = strings.TrimPrefix(name, outer.Name + ".")
							return
						}
						name = snap.NeedImport(stack[1].Pos(), "", outer.Obj.(*types.PkgName).Imported()) + strings.TrimPrefix(name, outer.Name)
					}
				}
			}
			if call, ok := stack[1].(*ast.CallExpr); ok && call.Fun == id {
				fixName()
				snap.InsertAt(call.Lparen+1, name+", ")
				return
			} else if sel, ok := stack[1].(*ast.SelectorExpr); ok && sel.Sel == id {
				if call, ok := stack[2].(*ast.CallExpr); ok && call.Fun == stack[1] {
					fixName()
					snap.InsertAt(call.Lparen+1, name+", ")
					return
				}
			}
		})
	})

	return nil
}
