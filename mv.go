// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"path/filepath"
	"reflect"
	"unicode"
	"unicode/utf8"

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

func cmdMv(snap *refactor.Snapshot, args []string) (morePkgs []string, changesExports bool, err error) {
	if len(args) < 2 {
		return nil, false, fmt.Errorf("usage: mv old... new")
	}

	var items []*refactor.Item
	for _, arg := range args {
		items = append(items, snap.Lookup(arg))
	}
	for i, item := range items[:len(items)-1] {
		if item == nil {
			return nil, false, fmt.Errorf("cannot find %s", args[i])
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
			return nil, false, fmt.Errorf("cannot move %s to directory %s", item.Kind, dst.Name)
		}

		if len(srcs) == 1 && srcs[0].Outer == nil && srcs[0].Kind != refactor.ItemFile && dst.Kind == refactor.ItemFile {
			mvCode(snap, srcs[0], dst.Name)
			return nil, false, nil
		}
		if len(srcs) == 1 && srcs[0].Outer == nil && srcs[0].Kind != refactor.ItemFile && dst.Kind == refactor.ItemDir {
			for _, pkg := range snap.Packages() {
				if pkg.PkgPath == dst.Name {
					mvCodePkg(snap, srcs[0], pkg)
					return nil, false, nil
				}
			}
			return []string{dst.Name}, false, nil
		}
	}

	// Otherwise, renaming to program identifier, which must not exist.
	if len(items) != 2 {
		return nil, false, fmt.Errorf("cannot move multiple items to %s", args[len(args)-1])
	}

	old, newItem := items[0], items[1]
	oldPath, newPath := args[0], args[1]
	if newItem != nil {
		return nil, false, fmt.Errorf("already have %s", newPath)
	}

	var newOuter *refactor.Item
	newPrefix, newName, ok := cutLast(newPath, ".")
	if ok {
		newOuter = snap.Lookup(newPrefix)
		if newOuter == nil {
			return nil, false, fmt.Errorf("cannot find %s", newPrefix)
		}
	} else {
		newName = newPath
	}

	// Check that newName is a valid identifier.
	// (Arbitrary syntax would make simple string search for dots invalid, among other problems.)
	if !isGoIdent.MatchString(newName) {
		return nil, false, fmt.Errorf("malformed replacement: not a valid Go identifier: %s", newName)
	}

	r, _ := utf8.DecodeRuneInString(oldPath)
	exported := unicode.IsUpper(r)

	// Rename of global.
	if old.Outer == nil && newOuter == nil {
		rewriteDefn(snap, old, newName)
		rewriteUses(snap, old, newName, notInScope(newName))
		return nil, exported, nil
	}

	// Rename of struct field or method.
	if old.Outer != nil && newOuter != nil && old.Outer.Obj == newOuter.Obj {
		rewriteDefn(snap, old, newName)
		rewriteUses(snap, old, newName, nil)
		return nil, exported, nil
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

			return nil, exported, nil
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
		return nil, exported, nil
	}

	// TODO: Rename variable in function to global.

	// TODO: Rename global to variable in function.

	return nil, false, fmt.Errorf("unimplemented replacement: %v -> %v . %v", old, newOuter, newName)
}

func rewriteDefn(snap *refactor.Snapshot, old *refactor.Item, new string) {
	stack := snap.FindAST(old.Obj.Pos())
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
		refactor.InspectAST(file, func(stack []ast.Node) {
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
	stack := snap.FindAST(old.Obj.Pos())
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
	stack := snap.FindAST(structPos)

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
	stack := snap.FindAST(method.Pos()) // FuncType Ident FuncDecl
	decl := stack[2].(*ast.FuncDecl)
	snap.Edit(decl.Recv.Opening, decl.Recv.Opening, " "+name)

	// Drop ) MethodName( from declaration, replacing with comma if there are arguments.
	sep := ""
	if len(decl.Type.Params.List) > 0 {
		sep = ", "
	}
	snap.Edit(decl.Recv.Closing, decl.Type.Params.Opening+1, sep)

	// TODO: Need to add names to receiver or params if some were named and others not.

	// Find and convert method uses.
	// But first, is this a pointer method or a value method?
	sig := method.Type().(*types.Signature)
	recvType := sig.Recv().Type()
	_, recvPtr := recvType.(*types.Pointer)

	snap.ForEachFile(func(pkg *packages.Package, file *ast.File) {
		refactor.InspectAST(file, func(stack []ast.Node) {
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

			// TODO: walk past parens
			call, _ := stack[2].(*ast.CallExpr)
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
					// TODO parens
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
				snap.Edit(sel.X.Pos(), sel.X.Pos(), "&")
			}
			if selPtr && !recvPtr {
				snap.Edit(sel.X.Pos(), sel.X.Pos(), "*")
			}
		})
	})
}

func mvCode(snap *refactor.Snapshot, old *refactor.Item, targetFile string) {
	// Find target file.
	var dst token.Pos
	for _, file := range snap.Target().Syntax {
		p := snap.Position(file.Package)
		if filepath.Base(p.Filename) == targetFile {
			dst = file.End()
			break
		}
	}
	if dst == token.NoPos {
		snap.ErrorAt(token.NoPos, "cannot find file")
		return
	}

	stack := snap.FindAST(old.Obj.Pos())
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
	refactor.InspectAST(decl, func(stack []ast.Node) {
		if id, ok := stack[0].(*ast.Ident); ok {
			if p, ok := snap.Target().TypesInfo.Uses[id].(*types.PkgName); ok {
				println("ID", p.Id())
				snap.NeedImport(dst, p.Id(), p.Imported().Path())
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

	stack := snap.FindAST(old.Obj.Pos())
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
	text := snap.Text(decl.Pos(), decl.End())
	buf := edit.NewBuffer(text)
	pkg := snap.Target().Types
	refactor.InspectAST(decl, func(stack []ast.Node) {
		if id, ok := stack[0].(*ast.Ident); ok {
			obj := snap.Target().TypesInfo.Uses[id]
			if pn, ok := obj.(*types.PkgName); ok {
				if pn.Imported().Path() == targetPkg.Types.Path() {
					sel := stack[1].(*ast.SelectorExpr)
					buf.Replace(int(sel.Pos()-decl.Pos()), int(sel.Sel.Pos()-decl.Pos()), "")
				} else {
					snap.NeedImport(dst, pn.Id(), pn.Imported().Path())
				}
			} else if obj != nil && obj.Parent() == pkg.Scope() {
				off := int(id.Pos() - decl.Pos())
				snap.NeedImport(dst, pkg.Name(), pkg.Path())
				buf.Replace(off, off, pkg.Name()+".")
			}
		}
	})

	// Move code.
	snap.Edit(decl.Pos(), decl.End(), "")
	snap.Edit(dst, dst, "\n"+buf.String())

	// TODO: Delete unused imports at some point.
}
