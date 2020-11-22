// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"go/ast"
	"go/token"
	"go/types"
	"strings"

	"rsc.io/rf/refactor"
)

func cmdInline(snap *refactor.Snapshot, args string) {
	args = strings.TrimLeft(args, " \t")
	var rm map[types.Object]bool
	if flag, rest, _ := cutAny(args, " \t"); flag == "-rm" {
		rm = make(map[types.Object]bool)
		args = rest
	}

	items, _ := snap.EvalList(args)
	if len(items) == 0 {
		snap.ErrorAt(token.NoPos, "usage: inline [-rm] decl...\n")
		return
	}

	fix := make(map[types.Object]ast.Expr)

	for _, item := range items {
		if item == nil {
			continue
		}
		switch item.Kind {
		default:
			snap.ErrorAt(token.NoPos, "inline %s: %v not supported", item.Name, item.Kind)
			continue
		case refactor.ItemConst, refactor.ItemType, refactor.ItemVar:
			obj := item.Obj
			stack := snap.SyntaxAt(obj.Pos())
			id := stack[0].(*ast.Ident)
			spec := stack[1].(*ast.ValueSpec)
			decl := stack[2].(*ast.GenDecl)
			for i := range spec.Names {
				if spec.Names[i] == id {
					if i >= len(spec.Values) {
						snap.ErrorAt(id.Pos(), "no initializer for %s", item.Name)
						continue
					}
					fix[obj] = spec.Values[i]
					if rm != nil {
						rm[obj] = true
					}
				}
			}
			// TODO rm
			_ = rm
			_ = decl
		}
	}

	inlineValues(snap, fix)
	if rm != nil {
		removeDecls(snap, rm)
	}

	// TODO: Should we inline in other packages?
}

func inlineValues(snap *refactor.Snapshot, fix map[types.Object]ast.Expr) {
	snap.ForEachFile(func(pkg *refactor.Package, file *ast.File) {
		refactor.Walk(file, func(stack []ast.Node) {
			id, ok := stack[0].(*ast.Ident)
			if !ok {
				return
			}
			obj := pkg.TypesInfo.Uses[id]
			repl := fix[obj]
			if repl == nil {
				return
			}
			text := string(snap.Text(repl.Pos(), repl.End()))
			snap.ReplaceNode(id, transplant(snap, text, repl.Pos(), id.Pos(), nil))
		})
	})
}
