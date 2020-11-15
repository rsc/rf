// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"reflect"
	"strings"

	"golang.org/x/tools/go/packages"
	"rsc.io/rf/edit"
	"rsc.io/rf/refactor"
)

type posChecker func(snap *refactor.Snapshot, stack []ast.Node)

func notInScope(name string) posChecker {
	return func(snap *refactor.Snapshot, stack []ast.Node) {
		if _, ok := stack[1].(*ast.SelectorExpr); ok {
			// Rewriting after a dot, so scope is not a concern.
			return
		}
		pos := stack[0].Pos()
		if snap.LookupAt(name, pos) != nil {
			snap.ErrorAt(pos, "%s already in scope", name)
		}
	}
}

func inScope(name string, obj types.Object) posChecker {
	return func(snap *refactor.Snapshot, stack []ast.Node) {
		if _, ok := stack[1].(*ast.SelectorExpr); ok {
			// Rewriting after a dot, so scope is not a concern.
			return
		}
		pos := stack[0].Pos()
		if snap.LookupAt(name, pos) != obj {
			snap.ErrorAt(pos, "%s is shadowed", name)
		}
	}
}

func cmdMv(snap *refactor.Snapshot, argsText string) (more []string, exp bool) {
	args := strings.Fields(argsText)
	if len(args) < 2 {
		snap.ErrorAt(token.NoPos, "usage: mv old... new")
		return
	}

	var items []*refactor.Item
	for _, arg := range args {
		items = append(items, snap.Lookup(arg))
	}
	for i, item := range items[:len(items)-1] {
		if item == nil {
			snap.ErrorAt(token.NoPos, "cannot find %s", args[i])
			return
		}
	}

	srcs, dst := items[:len(items)-1], items[len(items)-1]
	if dst != nil && (dst.Kind == refactor.ItemDir || dst.Kind == refactor.ItemFile) {
		for _, item := range srcs {
			if item.Outer == nil && item.Kind != refactor.ItemDir {
				// ok
				continue
			}
			if item.Kind == refactor.ItemMethod && item.Outer.Kind == refactor.ItemType {
				// ok
				continue
			}
			snap.ErrorAt(token.NoPos, "cannot move %s to directory %s", item.Kind, dst.Name)
			return
		}

		if len(srcs) == 1 && srcs[0].Outer == nil && srcs[0].Kind != refactor.ItemFile && dst.Kind == refactor.ItemFile {
			mvCode(snap, srcs[0], dst.Name)
			return
		}
		if len(srcs) == 1 && srcs[0].Outer == nil && srcs[0].Kind != refactor.ItemFile && dst.Kind == refactor.ItemDir {
			for _, pkg := range snap.Packages() {
				if pkg.PkgPath == dst.Name {
					mvCodePkg(snap, srcs[0], pkg)
					return
				}
			}
			return []string{dst.Name}, false
		}
	}

	// Otherwise, renaming to program identifier, which must not exist.
	if len(items) != 2 {
		snap.ErrorAt(token.NoPos, "cannot move multiple items to %s", args[len(args)-1])
		return
	}

	old, newItem, newPath := items[0], items[1], args[1]
	if newItem != nil {
		snap.ErrorAt(newItem.Obj.Pos(), "already have %s", newPath)
		return
	}

	var newOuter *refactor.Item
	newPrefix, newName, ok := cutLast(newPath, ".")
	if ok {
		newOuter = snap.Lookup(newPrefix)
		if newOuter == nil {
			snap.ErrorAt(token.NoPos, "cannot find destination %s", newPrefix)
			return
		}
	} else {
		newName = newPath
	}

	// Check that newName is a valid identifier.
	// (Arbitrary syntax would make simple string search for dots invalid, among other problems.)
	if !isGoIdent.MatchString(newName) {
		snap.ErrorAt(token.NoPos, "malformed replacement: not a valid Go identifier: %s", newName)
		return
	}

	// Rename of global.
	if old.Outer == nil && newOuter == nil {
		rewriteDefn(snap, old, newName)
		rewriteUses(snap, old, newName, notInScope(newName))
		exp = token.IsExported(old.Name)
		return
	}

	// Rename of struct field or method.
	if old.Outer != nil && newOuter != nil && old.Outer.Obj == newOuter.Obj {
		rewriteDefn(snap, old, newName)
		rewriteUses(snap, old, newName, nil)
		_, last, _ := cutLast(old.Name, ".")
		exp = token.IsExported(last)
		return
	}

	// Rename global var to new field in global var of type struct.
	newTop := newOuter.Outermost()
	if old.Outer == nil && old.Kind == refactor.ItemVar &&
		newTop != nil && newTop.Kind == refactor.ItemVar &&
		(newOuter.Kind == refactor.ItemVar || newOuter.Kind == refactor.ItemField) {
		tvar := newOuter.Obj.(*types.Var)
		if _, ok := tvar.Type().Underlying().(*types.Struct); ok {
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

			removeDecl(snap, old)
			addStructField(snap, structPos, newName, old.Obj.Type())
			rewriteUses(snap, old, newPath, inScope(newTop.Name, newTop.Obj))
			exp = token.IsExported(old.Name)
			return
		}
	}

	// TODO: Rename field in global struct var to plain global var.

	// TODO: Rename global function to method.
	if old.Outer == nil && old.Kind == refactor.ItemFunc &&
		newOuter != nil && newOuter.Kind == refactor.ItemType {
		if _, ok := newOuter.Obj.(*types.TypeName); ok {
			// TODO check method set for newName
			// TODO check that first argument of old function is receiver
			// TODO finish
		}
	}

	// Rename method to global function.
	if old.Kind == refactor.ItemMethod && old.Outer.Outer == nil && old.Outer.Kind == refactor.ItemType && newOuter == nil {
		methodToFunc(snap, old.Obj.(*types.Func), newName)
		_, last, _ := cutLast(old.Name, ".")
		exp = token.IsExported(last)
		return
	}

	// TODO: Rename variable in function to global.

	// TODO: Rename global to variable in function.

	snap.ErrorAt(token.NoPos, "unimplemented replacement: %v -> %v . %v", old, newOuter, newName)
	return
}

func rewriteDefn(snap *refactor.Snapshot, old *refactor.Item, new string) {
	stack := snap.SyntaxAt(old.Obj.Pos())
	// For a function declaration, the FuncType ends up spuriously on the stack.
	// (It is considered to start at the func keyword and end after the results,
	// so it sits both before and after the Ident, and it is walked after the Ident.)
	if _, ok := stack[0].(*ast.FuncType); ok {
		stack = stack[1:]
	}
	id, ok := stack[0].(*ast.Ident)
	if !ok {
		var types []string
		for _, n := range stack {
			types = append(types, fmt.Sprintf("%T", n))
		}
		snap.ErrorAt(old.Obj.Pos(), "did not find definition - %v", types)
		return
	}
	snap.Edit(id.Pos(), id.End(), new)
}

func rewriteUses(snap *refactor.Snapshot, old *refactor.Item, new string, checkPos posChecker) {
	snap.ForEachFile(func(pkg *packages.Package, file *ast.File) {
		refactor.Walk(file, func(stack []ast.Node) {
			id, ok := stack[0].(*ast.Ident)
			if !ok || pkg.TypesInfo.Uses[id] != old.Obj {
				return
			}
			if checkPos != nil {
				checkPos(snap, stack)
			}
			snap.Edit(id.Pos(), id.End(), new)
		})
	})
}

func StackTypes(list []ast.Node) string {
	var types []reflect.Type
	for _, n := range list {
		types = append(types, reflect.TypeOf(n))
	}
	return fmt.Sprint(types)
}

// TODO: Return doc comments.
func removeDecl(snap *refactor.Snapshot, old *refactor.Item) {
	stack := snap.SyntaxAt(old.Obj.Pos())
	// stack is *ast.Ident *ast.ValueSpec *ast.GenDecl
	spec := stack[1].(*ast.ValueSpec)
	decl := stack[2].(*ast.GenDecl)

	if len(decl.Specs) == 1 && len(spec.Names) == 1 {
		// Delete entire declaration.
		// TODO: Doc comments too.
		// TODO: Newline too.
		snap.Edit(decl.Pos(), decl.End(), "")
		return
	}

	if len(spec.Names) == 1 {
		snap.Edit(spec.Pos(), spec.End(), "")
		return
	}

	for i, id := range spec.Names {
		if id.Pos() == old.Obj.Pos() {
			if i == 0 {
				snap.Edit(id.Pos(), spec.Names[i+1].Pos(), "")
			} else {
				snap.Edit(spec.Names[i-1].End(), id.End(), "")
			}
			// TODO: Deal with initializer spec.Values[i]
			return
		}
	}

	snap.ErrorAt(old.Obj.Pos(), "could not find declaration to delete")
}

func addStructField(snap *refactor.Snapshot, structPos token.Pos, name string, typ types.Type) {
	stack := snap.SyntaxAt(structPos)

	var xtyp ast.Expr
Loop:
	for _, n := range stack {
		switch n := n.(type) {
		case *ast.Field:
			xtyp = n.Type
			break Loop
		case *ast.ValueSpec:
			xtyp = n.Type
			break Loop
		case *ast.TypeSpec:
			xtyp = n.Type
			break Loop
		}
	}

	styp, ok := xtyp.(*ast.StructType)
	if !ok {
		snap.ErrorAt(structPos, "cannot find struct to update")
		return
	}
	fields := styp.Fields

	// Insert field at end of struct.
	// If closing } is not on a line by itself, move it to one.
	line := func(pos token.Pos) int {
		return snap.Position(pos).Line
	}
	if line(fields.Opening) == line(fields.Closing) ||
		len(fields.List) > 0 && line(fields.List[len(fields.List)-1].End()) == line(fields.Closing) {
		snap.Edit(fields.Closing, fields.Closing, "\n")
	}
	snap.Edit(fields.Closing, fields.Closing, name+" "+typ.String()+"\n")
}

func methodToFunc(snap *refactor.Snapshot, method *types.Func, name string) {
	// Convert method declaration.
	// Insert name before receiver list.
	stack := snap.SyntaxAt(method.Pos()) // FuncType Ident FuncDecl
	decl := stack[2].(*ast.FuncDecl)
	snap.Edit(decl.Recv.Opening, decl.Recv.Opening, " "+name)

	// Drop ) MethodName( from declaration, replacing with comma if there are arguments.
	sep := ""
	if len(decl.Type.Params.List) > 0 {
		sep = ", "
	}
	snap.Edit(decl.Recv.Closing, decl.Type.Params.Opening+1, sep)

	// If receiver is named but params are not, or vice versa,
	// need to add _ to each name.
	if len(decl.Type.Params.List) > 0 &&
		(len(decl.Recv.List[0].Names) == 0) != (len(decl.Type.Params.List[0].Names) == 0) {
		if len(decl.Recv.List[0].Names) == 0 {
			pos := decl.Recv.List[0].Type.Pos()
			snap.Edit(pos, pos, "_ ")
		} else {
			for _, f := range decl.Type.Params.List {
				pos := f.Type.Pos()
				snap.Edit(pos, pos, "_ ")
			}
		}
	}

	// Find and convert method uses.
	// But first, is this a pointer method or a value method?
	sig := method.Type().(*types.Signature)
	recvType := sig.Recv().Type()
	_, recvPtr := recvType.(*types.Pointer)

	snap.ForEachFile(func(pkg *packages.Package, file *ast.File) {
		refactor.Walk(file, func(stack []ast.Node) {
			id, ok := stack[0].(*ast.Ident)
			if !ok || pkg.TypesInfo.Uses[id] != method {
				return
			}

			// Found a use to convert. Can we refer to the new name here?
			// TODO: Add import
			if snap.LookupAt(name, id.Pos()) != nil {
				snap.ErrorAt(id.Pos(), "%s already in scope", name)
			}

			// But what kind of use?
			// Determine selector type, whether it's a type or value (T.M vs x.M),
			// and whether it's being called.
			sel := stack[1].(*ast.SelectorExpr)
			tv, ok := pkg.TypesInfo.Types[sel.X]
			if !ok {
				snap.ErrorAt(sel.Pos(), "lost type information")
				return
			}
			selType := tv.IsType()
			_, selPtr := tv.Type.(*types.Pointer)

			call, _ := stack[nonParen(stack, 2)].(*ast.CallExpr)
			if call == nil {
				// Method value.
				if !selType { // x.M
					snap.ErrorAt(id.Pos(), "cannot rewrite method value to function")
					return
				}
				if selPtr && !recvPtr { // (*T).F but F is a value method - valid Go but too hard to convert
					snap.ErrorAt(id.Pos(), "cannot rewrite pointer method value (with value receiver method) to function")
					return
				}
				snap.Edit(sel.Pos(), sel.End(), name)
				return
			}

			// Call of method expression?
			if selType {
				// T.M(rcvr, x) -> newname(rcvr, x).
				snap.Edit(call.Pos(), call.Lparen, name)
				if selPtr && !recvPtr { // (*T).M(rcvr, x) -> newname(*rcvr, x)
					// There are no binary operators that can yield a pointer,
					// so no possible need for parens around rcvr.
					snap.Edit(call.Lparen+1, call.Lparen+1, "*")
				}
				return
			}

			// Call of method with real receiver.
			// Turn x.M(y) into name(x, y).
			// May need to turn x into &x or *x as well.
			snap.Edit(sel.X.Pos(), sel.X.Pos(), name+"(")
			snap.Edit(sel.X.End(), call.Lparen+1, "")
			if len(call.Args) > 0 {
				snap.Edit(call.Lparen+1, call.Lparen+1, ", ")
			}
			if recvPtr && !selPtr {
				// There are no binary operators that can yield an addressable expression,
				// so no possible need for parens around x..
				snap.Edit(sel.X.Pos(), sel.X.Pos(), "&")
			}
			if selPtr && !recvPtr {
				// There are no binary operators that can yield a pointer,
				// so no possible need for parens around x.
				snap.Edit(sel.X.Pos(), sel.X.Pos(), "*")
			}
		})
	})
}

func nonParen(stack []ast.Node, i int) int {
	for ; i < len(stack); i++ {
		if _, ok := stack[i].(*ast.ParenExpr); !ok {
			break
		}
	}
	return i
}

func mvCode(snap *refactor.Snapshot, old *refactor.Item, targetFile string) {
	// Find target file.
	_, file := snap.TargetFileByName(targetFile)
	if file == nil {
		snap.ErrorAt(token.NoPos, "cannot find file %s in target packages", targetFile)
		return
	}
	dst := file.End()

	srcPkg := snap.PackageAt(old.Obj.Pos())
	stack := snap.SyntaxAt(old.Obj.Pos())
	var decl ast.Node
	switch n := stack[2].(type) {
	case *ast.GenDecl:
		decl = n
	case *ast.FuncDecl:
		decl = n
	default:
		snap.ErrorAt(old.Obj.Pos(), "unknown declaration type %T", n)
	}

	// Make sure target file has the necessary imports.
	refactor.Walk(decl, func(stack []ast.Node) {
		if id, ok := stack[0].(*ast.Ident); ok {
			if p, ok := srcPkg.TypesInfo.Uses[id].(*types.PkgName); ok {
				snap.NeedImport(dst, p.Name(), p.Imported().Path())
			}
		}
	})

	// Move code.
	text := snap.Text(decl.Pos(), decl.End())
	snap.Edit(decl.Pos(), decl.End(), "")
	snap.Edit(dst, dst, "\n"+string(text))

	// TODO: Delete unused imports at some point.
}

func mvCodePkg(snap *refactor.Snapshot, old *refactor.Item, targetPkg *packages.Package) {
	// Find target package.
	dst := targetPkg.Syntax[0].End()
	if dst == token.NoPos {
		snap.ErrorAt(token.NoPos, "cannot find pkg")
		return
	}

	stack := snap.SyntaxAt(old.Obj.Pos())
	var decl ast.Node
	switch n := stack[2].(type) {
	case *ast.GenDecl:
		decl = n
	case *ast.FuncDecl:
		decl = n
	default:
		snap.ErrorAt(old.Obj.Pos(), "unknown declaration type %T", n)
	}

	// Make sure target package has the necessary imports.
	// Remove references to the target package itself.
	// Insert references to the original package as needed.
	// TODO: Something about import cycles.
	pkg := snap.PackageAt(decl.Pos())
	text := snap.Text(decl.Pos(), decl.End())
	buf := edit.NewBuffer(text)
	refactor.Walk(decl, func(stack []ast.Node) {
		if id, ok := stack[0].(*ast.Ident); ok {
			obj := pkg.TypesInfo.Uses[id]
			if pn, ok := obj.(*types.PkgName); ok {
				if pn.Imported().Path() == targetPkg.Types.Path() {
					sel := stack[1].(*ast.SelectorExpr)
					buf.Replace(int(sel.Pos()-decl.Pos()), int(sel.Sel.Pos()-decl.Pos()), "")
				} else {
					snap.NeedImport(dst, pn.Id(), pn.Imported().Path())
				}
			} else if obj != nil && obj.Parent() == pkg.Types.Scope() {
				off := int(id.Pos() - decl.Pos())
				snap.NeedImport(dst, pkg.Types.Name(), pkg.Types.Path())
				buf.Replace(off, off, pkg.Types.Name()+".")
			}
		}
	})

	// Move code.
	snap.Edit(decl.Pos(), decl.End(), "")
	snap.Edit(dst, dst, "\n"+buf.String())

	// Update references to what was moved.
	rewriteUses(snap, old, targetPkg.Types.Name()+"."+old.Name, nil)

	// TODO: Delete unused imports at some point.
}
