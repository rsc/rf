// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"fmt"
	"go/ast"
	"go/token"
	"strings"

	"rsc.io/rf/refactor"
)

func cmdAdd(snap *refactor.Snapshot, args string) error {
	return cmdAddSub(snap, "add", args)
}

func cmdSub(snap *refactor.Snapshot, args string) error {
	return cmdAddSub(snap, "sub", args)
}

func cmdAddSub(snap *refactor.Snapshot, cmd, args string) error {
	item, expr, text := snap.EvalNext(args)
	if expr == "" {
		return newErrUsage("%s address text...", cmd)
	}
	if item == nil {
		// Error already reported.
		return nil
	}

	var pos, end token.Pos
	switch item.Kind {
	default:
		return fmt.Errorf("TODO: %s after %s", cmd, item.Kind)

	case refactor.ItemNotFound:
		return newErrPrecondition("%s not found", item.Name)

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
			return fmt.Errorf("no such directory %s", item.Name)
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
	return nil
}
