// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"go/ast"
	"go/token"

	"golang.org/x/tools/go/packages"
	"rsc.io/rf/refactor"
)

func cmdAdd(snap *refactor.Snapshot, args string) (more []string, exp bool) {
	item, expr, text := snap.LookupNext(args)
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
		srcPkg := snap.Targets()[0] // TODO
		srcFile := findFile(snap, srcPkg, item.Name)
		_, pos = snap.FileRange(srcFile.Package)

	case refactor.ItemDir:
		var dstPkg *packages.Package
		for _, pkg := range snap.Packages() {
			if pkg.PkgPath == item.Name {
				dstPkg = pkg
				break
			}
		}
		if dstPkg == nil {
			return []string{item.Name}, false
		}
		_, pos = snap.FileRange(dstPkg.Syntax[0].Package)

	case refactor.ItemPos:
		pos = item.End
	}

	// TODO: Is final \n a good idea?
	snap.InsertAt(pos, text+"\n")
	return nil, false
}
