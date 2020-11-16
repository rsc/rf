// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/token"
	"go/types"

	"rsc.io/rf/refactor"
)

func mvStmt(snap *refactor.Snapshot, old *refactor.Item, name string) {
	stack := snap.SyntaxAt(old.Pos)
	if _, ok := stack[0].(*ast.Ident); ok {
		stack = stack[1:]
	}
	block := stack[0].(*ast.BlockStmt)

	list := block.List
	for len(list) > 0 && list[0].End() < old.Pos {
		list = list[1:]
	}
	for len(list) > 0 && list[len(list)-1].Pos() >= old.End {
		list = list[:len(list)-1]
	}

	srcPkg, _ := snap.FileAt(old.Pos)
	fn := stack[1].(*ast.FuncDecl)
	didParam := make(map[types.Object]bool)
	didResult := make(map[types.Object]bool)
	var params []types.Object
	var results []types.Object
	wrote := make(map[types.Object]bool)
	refactor.Walk(fn, func(stack []ast.Node) {
		id, ok := stack[0].(*ast.Ident)
		if !ok {
			return
		}
		if id.End() < old.Pos {
			return
		}
		if _, ok := stack[1].(*ast.SelectorExpr); ok {
			return
		}
		obj := srcPkg.TypesInfo.Uses[id]
		if obj == nil || obj.Parent() == types.Universe || obj.Parent() == srcPkg.Types.Scope() {
			return
		}

		if obj.Pos() < old.Pos && id.End() < old.End {
			if !didParam[obj] {
				didParam[obj] = true
				params = append(params, obj)
			}
		}
		if id.End() < old.End {
			if as, ok := stack[1].(*ast.AssignStmt); ok && as.Tok == token.ASSIGN {
				for _, n := range as.Lhs {
					if nid, ok := n.(*ast.Ident); ok {
						wrote[srcPkg.TypesInfo.Uses[nid]] = true
					}
				}
			}
		}
		if (old.Pos <= obj.Pos() && obj.Pos() < old.End || wrote[obj]) && old.End < id.Pos() {
			if !didResult[obj] {
				didResult[obj] = true
				results = append(results, obj)
			}
		}
	})

	var buf bytes.Buffer
	fmt.Fprintf(&buf, "\n\nfunc %s(", name)
	for i, obj := range params {
		if i > 0 {
			fmt.Fprintf(&buf, ", ")
		}
		fmt.Fprintf(&buf, "%s %s", obj.Name(), obj.Type())
	}
	fmt.Fprintf(&buf, ")")
	if len(results) > 0 {
		if len(results) == 1 {
			fmt.Fprintf(&buf, " %s", results[0].Type())
		} else {
			fmt.Fprintf(&buf, " (")
			for i, obj := range results {
				if i > 0 {
					fmt.Fprintf(&buf, ", ")
				}
				fmt.Fprintf(&buf, "%s", obj.Type())
			}
			fmt.Fprintf(&buf, ")")
		}
	}
	fmt.Fprintf(&buf, " {\n")
	buf.Write(snap.Text(old.Pos, old.End))
	if len(results) > 0 {
		fmt.Fprintf(&buf, "\treturn ")
		for i, obj := range results {
			if i > 0 {
				fmt.Fprintf(&buf, ", ")
			}
			fmt.Fprintf(&buf, "%s", obj.Name())
		}
		fmt.Fprintf(&buf, "\n")
	}
	fmt.Fprintf(&buf, "}\n\n")

	snap.InsertAt(fn.End(), buf.String())

	buf.Reset()
	for i, obj := range results {
		if i > 0 {
			fmt.Fprintf(&buf, ", ")
		}
		fmt.Fprintf(&buf, "%s", obj.Name())
	}
	if len(results) > 0 {
		fmt.Fprintf(&buf, " := ")
	}
	fmt.Fprintf(&buf, "%s(", name)
	for i, obj := range params {
		if i > 0 {
			fmt.Fprintf(&buf, ", ")
		}
		fmt.Fprintf(&buf, "%s", obj.Name())
	}
	fmt.Fprintf(&buf, ")\n")

	snap.ReplaceAt(old.Pos, old.End, buf.String())

	//	panic(StackTypes(stack))
}
