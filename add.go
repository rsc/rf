// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"go/ast"
	"go/token"
	"strings"

	"rsc.io/rf/refactor"
)

func cmdAdd(snap *refactor.Snapshot, args string) {
	cmdAddSub(snap, "add", args)
}

func cmdSub(snap *refactor.Snapshot, args string) {
	cmdAddSub(snap, "sub", args)
}

func cmdAddSub(snap *refactor.Snapshot, cmd, args string) {
	item, expr, text := snap.EvalNext(args)
	if expr == "" {
		snap.ErrorAt(token.NoPos, "usage: %s address text...\n", cmd)
		return
	}
	if item == nil {
		// Error already reported.
		return
	}

	var pos, end token.Pos
	switch item.Kind {
	default:
		snap.ErrorAt(token.NoPos, "TODO: %s after %s", cmd, item.Kind)

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
		pos, end = nodeRange(snap, after)

	case refactor.ItemFile:
		_, srcFile := snap.FileByName(item.Name)
		pos, end = snap.FileRange(srcFile.Package)

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
		pos, end = snap.FileRange(dstPkg.Files[0].Syntax.Pos())

	case refactor.ItemPos:
		pos, end = item.Pos, item.End
	}

	var old string
	if cmd == "sub" {
		old = string(snap.Text(pos, end))
		snap.DeleteAt(pos, end)
	}
	if cmd == "add" || strings.HasSuffix(old, "\n") {
		// TODO: Is final \n a good idea?
		text += "\n"
	}
	snap.InsertAt(end, text)
}
