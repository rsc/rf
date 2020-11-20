// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"go/ast"
	"go/token"
	"go/types"

	"rsc.io/rf/refactor"
)

func cmdKey(snap *refactor.Snapshot, args string) {
	items, _ := snap.LookupAll(args)
	if len(items) == 0 {
		snap.ErrorAt(token.NoPos, "usage: key StructType...")
		return
	}

	var fixing []types.Type
	for _, item := range items {
		if item == nil {
			continue
		}
		if item.Kind != refactor.ItemType {
			snap.ErrorAt(token.NoPos, "%s is not a type", item.Name)
			continue
		}
		typ := item.Obj.(*types.TypeName).Type().(*types.Named)
		if _, ok := typ.Underlying().(*types.Struct); !ok {
			snap.ErrorAt(token.NoPos, "%s is not a struct type", item.Name)
			continue
		}
		fixing = append(fixing, typ)
	}
	if snap.Errors() > 0 {
		return
	}

	keyLiterals(snap, fixing)
}

func keyLiterals(snap *refactor.Snapshot, list []types.Type) {
	fixing := make(map[types.Type]bool)
	for _, t := range list {
		fixing[t] = true
	}

	snap.ForEachFile(func(pkg *refactor.Package, file *ast.File) {
		refactor.Walk(file, func(stack []ast.Node) {
			lit, ok := stack[0].(*ast.CompositeLit)
			if !ok || len(lit.Elts) == 0 || lit.Incomplete {
				return
			}
			if _, ok := lit.Elts[0].(*ast.KeyValueExpr); ok {
				// already keyed
				return
			}
			typ := pkg.TypesInfo.TypeOf(lit)
			if !fixing[typ] {
				return
			}
			struc := typ.Underlying().(*types.Struct)
			if struc.NumFields() != len(lit.Elts) {
				snap.ErrorAt(lit.Pos(), "wrong number of struct literal initializers")
				return
			}
			for i, e := range lit.Elts {
				f := struc.Field(i)
				snap.InsertAt(e.Pos(), f.Name()+":")
			}
		})
	})
}
