// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"go/ast"
	"go/token"
	"strings"
	"unicode"

	"golang.org/x/tools/go/packages"
	"rsc.io/rf/refactor"
)

func cmdHide(snap *refactor.Snapshot, text string) (more []string, exp bool) {
	args := strings.Fields(text)
	if len(args) < 1 {
		snap.ErrorAt(token.NoPos, "usage: gude T.F...")
		return
	}

	var items []*refactor.Item
	for _, arg := range args {
		item := snap.Lookup(arg)
		if item == nil {
			snap.ErrorAt(token.NoPos, "cannot find %s", arg)
			continue
		}
		if item.Outermost().Kind != refactor.ItemType || item.Kind != refactor.ItemField {
			snap.ErrorAt(token.NoPos, "%s is not a struct field", item.Name)
		}
		items = append(items, item)
	}
	if snap.Errors() > 0 {
		return
	}

	for _, item := range items {
		_, name, _ := cut(item.Name, ".")
		if !token.IsExported(name) {
			snap.ErrorAt(token.NoPos, "%s is not exported", item.Name)
		}
		r := []rune(name)
		i := 0
		for i < len(r) && unicode.IsUpper(r[i]) {
			i++
		}
		if i > 1 {
			i--
		}
		for i--; i >= 0; i-- {
			r[i] = unicode.ToLower(r[i])
		}
		unexp := string(r)
		rewriteDefn(snap, item, unexp)
		addGetSet(snap, item)
	}

	exp = true
	return
}

func addGetSet(snap *refactor.Snapshot, item *refactor.Item) {
	snap.ForEachFile(func(pkg *packages.Package, file *ast.File) {
		refactor.Walk(file, func(stack []ast.Node) {
			id, ok := stack[0].(*ast.Ident)
			if !ok || pkg.TypesInfo.Uses[id] != item.Obj {
				return
			}
			if _, ok := stack[1].(*ast.KeyValueExpr); ok {
				// TODO
				return
			}
			sel, ok := stack[1].(*ast.SelectorExpr)
			if !ok {
				// TODO
				return
			}
			switch ctxt := stack[nonParen(stack, 2)].(type) {
			case *ast.UnaryExpr:
				if ctxt.Op == token.AND {
					// TODO
					return
				}
			case *ast.AssignStmt:
				for _, l := range ctxt.Lhs {
					if l == sel {
						// TODO
						return
					}
				}
			}
			snap.ReplaceNode(id, id.Name+"()")
		})
	})
}
