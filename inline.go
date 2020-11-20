// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"go/ast"
	"go/token"
	"go/types"
	"strings"

	"golang.org/x/tools/go/packages"
	"rsc.io/rf/refactor"
)

func cmdInline(snap *refactor.Snapshot, argsText string) (more []string, exp bool) {
	args := strings.Fields(argsText)
	var rm map[types.Object]bool
	if len(args) > 0 && args[0] == "-rm" {
		rm = make(map[types.Object]bool)
		args = args[1:]
	}
	if len(args) < 1 {
		snap.ErrorAt(token.NoPos, "usage: inline [-rm] decl...\n")
		return
	}

	fix := make(map[types.Object]ast.Expr)

	for _, arg := range args {
		item := snap.Lookup(arg)
		if item == nil {
			snap.ErrorAt(token.NoPos, "cannot find %s", arg)
			continue
		}
		switch item.Kind {
		default:
			snap.ErrorAt(token.NoPos, "inline %s: %v not supported", arg, item.Kind)
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
						snap.ErrorAt(id.Pos(), "no initializer for %s", arg)
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

	// TODO: If the names are exported, should we inline elsewhere too?
	// Probably. Certainly if they are being deleted.
	return nil, false
}

func inlineValues(snap *refactor.Snapshot, fix map[types.Object]ast.Expr) {
	snap.ForEachFile(func(pkg *packages.Package, file *ast.File) {
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
