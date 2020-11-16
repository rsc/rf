// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"go/token"
	"go/types"
	"strings"

	"rsc.io/rf/refactor"
)

func cmdAdd(snap *refactor.Snapshot, text string) (more []string, exp bool) {
	text = strings.TrimSpace(text)
	name, rest, _ := cutAny(text, " \t")
	prefix, name, ok := cut(name, ".")
	if !ok {
		snap.ErrorAt(token.NoPos, "add needs X.Y")
		return
	}
	outer := snap.Lookup(prefix)
	if outer.Kind == refactor.ItemVar || outer.Kind == refactor.ItemField {
		tvar := outer.Obj.(*types.Var)
		typ := tvar.Type().Underlying()
		if ptr, ok := typ.(*types.Pointer); ok {
			typ = ptr.Elem().Underlying()
		}
		if _, ok := typ.(*types.Struct); ok {
			// Finding struct type is a little tricky.
			// Except for empty structs, we could use the pos of the first field.
			// But an empty struct type has no pos at all, so we have to
			// use the pos of the declaration in which the struct appears.
			var structPos token.Pos
			switch typ := tvar.Type().(type) {
			case *types.Struct:
				structPos = tvar.Pos()
			case *types.Named:
				structPos = typ.Obj().Pos()
			}
			addStructField(snap, structPos, name, rest)
			return
		}
	}
	snap.ErrorAt(token.NoPos, "bad add")
	return
}
