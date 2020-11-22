// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"go/ast"
	"go/token"

	"rsc.io/rf/refactor"
)

func cmdAdd(snap *refactor.Snapshot, args string) {
	item, expr, text := snap.EvalNext(args)
	if expr == "" {
		snap.ErrorAt(token.NoPos, "usage: add address text...\n")
		return
	}
	if item == nil {
		// Error already reported.
		return
	}

	var pos token.Pos
	switch item.Kind {
	default:
		snap.ErrorAt(token.NoPos, "TODO: add after %s", item.Kind)

	case refactor.ItemConst, refactor.ItemFunc, refactor.ItemType, refactor.ItemVar, refactor.ItemField:
		stack := snap.SyntaxAt(item.Obj.Pos())
		if len(stack) == 0 {
			panic("LOST " + item.Name)
		}
		after := stack[1]
		switch after.(type) {
		case *ast.ValueSpec, *ast.TypeSpec:
			decl := stack[2].(*ast.GenDecl)
			if decl.Lparen == token.NoPos {
				after = decl
			}
		}
		_, pos = nodeRange(snap, after)

	case refactor.ItemFile:
		_, srcFile := snap.FileByName(item.Name)
		_, pos = snap.FileRange(srcFile.Package)

	case refactor.ItemDir:
		var dstPkg *refactor.Package
		for _, pkg := range snap.Packages() {
			if pkg.PkgPath == item.Name {
				dstPkg = pkg
				break
			}
		}
		if dstPkg == nil {
			return
		}
		_, pos = snap.FileRange(dstPkg.Files[0].Syntax.Pos())

	case refactor.ItemPos:
		pos = item.End
	}

	// TODO: Is final \n a good idea?
	snap.InsertAt(pos, text+"\n")
}
