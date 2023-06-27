// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"strings"

	"rsc.io/rf/refactor"
)

func cmdInline(snap *refactor.Snapshot, args string) error {
	args = strings.TrimLeft(args, " \t")
	var rm map[types.Object]bool
	if flag, rest, _ := cutAny(args, " \t"); flag == "-rm" {
		rm = make(map[types.Object]bool)
		args = rest
	}

	items, _ := snap.EvalList(args)
	if len(items) == 0 {
		return newErrUsage("inline [-rm] decl...")
	}

	fix := make(map[types.Object]ast.Expr)

	for _, item := range items {
		if item == nil {
			continue
		}
		switch item.Kind {
		default:
			return fmt.Errorf("inline %s: %v not supported", item.Name, item.Kind)
		case refactor.ItemNotFound:
			return newErrPrecondition("%s not found", item.Name)
		case refactor.ItemConst, refactor.ItemType, refactor.ItemVar:
			obj := item.Obj
			stack := snap.SyntaxAt(obj.Pos())
			id := stack[0].(*ast.Ident)
			decl := stack[2].(*ast.GenDecl)
			switch spec := stack[1].(type) {
			case *ast.TypeSpec:
				if spec.Assign == token.NoPos {
					snap.ErrorAt(id.Pos(), "%s is a defined type (not type alias)", item.Name)
					break
				}
				fix[obj] = spec.Type
				if rm != nil {
					rm[obj] = true
				}

			case *ast.ValueSpec:
				if spec.Type != nil {
					// TODO: Inline anyway, using explicit conversion as necessary.
					snap.ErrorAt(id.Pos(), "%s has declared type", item.Name)
					break
				}
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
	return nil
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
