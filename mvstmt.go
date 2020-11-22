// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/token"
	"go/types"

	"rsc.io/rf/refactor"
)

func mvStmt(snap *refactor.Snapshot, old *refactor.Item, name string) {
	stack := snap.SyntaxAt(old.Pos)
	if _, ok := stack[0].(*ast.Ident); ok {
		stack = stack[1:]
	}
	block := stack[0].(*ast.BlockStmt)

	list := block.List
	for len(list) > 0 && list[0].End() < old.Pos {
		list = list[1:]
	}
	for len(list) > 0 && list[len(list)-1].Pos() >= old.End {
		list = list[:len(list)-1]
	}

	srcPkg, srcFile := snap.FileAt(old.Pos)
	fn := stack[1].(*ast.FuncDecl)
	didParam := make(map[types.Object]bool)
	didResult := make(map[types.Object]bool)
	var params []types.Object
	var results []types.Object
	wrote := make(map[types.Object]bool)
	refactor.Walk(fn, func(stack []ast.Node) {
		id, ok := stack[0].(*ast.Ident)
		if !ok {
			return
		}
		if id.End() < old.Pos {
			return
		}
		obj := srcPkg.TypesInfo.Uses[id]
		if obj == nil || obj.Parent() == types.Universe || obj.Parent() == srcPkg.Types.Scope() {
			return
		}
		if _, ok := obj.(*types.PkgName); ok {
			return
		}
		if sel, ok := stack[1].(*ast.SelectorExpr); ok && id == sel.Sel {
			return
		}

		if obj.Pos() < old.Pos && id.End() < old.End {
			if !didParam[obj] {
				didParam[obj] = true
				params = append(params, obj)
			}
		}
		if id.End() < old.End {
			if as, ok := stack[1].(*ast.AssignStmt); ok && as.Tok == token.ASSIGN {
				for _, n := range as.Lhs {
					if nid, ok := n.(*ast.Ident); ok {
						wrote[srcPkg.TypesInfo.Uses[nid]] = true
					}
				}
			}
		}
		if (old.Pos <= obj.Pos() && obj.Pos() < old.End || wrote[obj]) && old.End < id.Pos() {
			if !didResult[obj] {
				didResult[obj] = true
				results = append(results, obj)
			}
		}
	})

	var buf bytes.Buffer
	fmtType := func(typ types.Type) {
		printType(&buf, snap, old.Pos, srcFile.End(), typ)
	}
	fmt.Fprintf(&buf, "\n\nfunc %s(", name)
	for i, obj := range params {
		if i > 0 {
			fmt.Fprintf(&buf, ", ")
		}
		fmt.Fprintf(&buf, "%s ", obj.Name())
		fmtType(obj.Type())
	}
	fmt.Fprintf(&buf, ")")
	if len(results) > 0 {
		if len(results) == 1 {
			fmt.Fprintf(&buf, " ")
			fmtType(results[0].Type())
		} else {
			fmt.Fprintf(&buf, " (")
			for i, obj := range results {
				if i > 0 {
					fmt.Fprintf(&buf, ", ")
				}
				fmtType(obj.Type())
			}
			fmt.Fprintf(&buf, ")")
		}
	}
	fmt.Fprintf(&buf, " {\n")
	buf.Write(snap.Text(old.Pos, old.End))
	if len(results) > 0 {
		fmt.Fprintf(&buf, "\treturn ")
		for i, obj := range results {
			if i > 0 {
				fmt.Fprintf(&buf, ", ")
			}
			fmt.Fprintf(&buf, "%s", obj.Name())
		}
		fmt.Fprintf(&buf, "\n")
	}
	fmt.Fprintf(&buf, "}\n\n")

	snap.InsertAt(fn.End(), buf.String())

	buf.Reset()
	for i, obj := range results {
		if i > 0 {
			fmt.Fprintf(&buf, ", ")
		}
		fmt.Fprintf(&buf, "%s", obj.Name())
	}
	if len(results) > 0 {
		fmt.Fprintf(&buf, " := ")
	}
	fmt.Fprintf(&buf, "%s(", name)
	for i, obj := range params {
		if i > 0 {
			fmt.Fprintf(&buf, ", ")
		}
		fmt.Fprintf(&buf, "%s", obj.Name())
	}
	fmt.Fprintf(&buf, ")\n")

	snap.ReplaceAt(old.Pos, old.End, buf.String())

	//	panic(StackTypes(stack))
}

func printType(b *bytes.Buffer, snap *refactor.Snapshot, src, dst token.Pos, typ types.Type) {
	switch typ := typ.(type) {
	default:
		snap.ErrorAt(src, "extract type %T not implemented", typ)
	case *types.Basic:
		// TODO check shadowing
		b.WriteString(typ.Name())
	case *types.Named:
		dstPkg, _ := snap.FileAt(dst)
		tn := typ.Obj()
		if tn.Pkg() != dstPkg.Types {
			name := snap.NeedImport(dst, "", tn.Pkg())
			b.WriteString(name + ".")
		}
		b.WriteString(tn.Name())
	case *types.Pointer:
		b.WriteString("*")
		printType(b, snap, src, dst, typ.Elem())
	case *types.Slice:
		b.WriteString("[]")
		printType(b, snap, src, dst, typ.Elem())
	case *types.Array:
		fmt.Fprintf(b, "[%d]", typ.Len())
		printType(b, snap, src, dst, typ.Elem())
	case *types.Chan:
		switch typ.Dir() {
		case types.SendRecv:
			b.WriteString("chan ")
		case types.SendOnly:
			b.WriteString("chan<- ")
		case types.RecvOnly:
			b.WriteString("<-chan ")
		}
		if typ.Dir() != types.SendOnly {
			if inner, ok := typ.Elem().(*types.Chan); ok && inner.Dir() == types.RecvOnly {
				b.WriteString("(")
				defer b.WriteString(")")
			}
		}
		printType(b, snap, src, dst, typ.Elem())
	case *types.Map:
		b.WriteString("map[")
		printType(b, snap, src, dst, typ.Key())
		b.WriteString("]")
		printType(b, snap, src, dst, typ.Elem())
	}
}
