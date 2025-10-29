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
	"path"
	"reflect"
	"regexp"
	"strings"

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

func cmdMv(snap *refactor.Snapshot, args string) error {
	args = strings.TrimSpace(args)
	if args == "" {
		return newErrUsage("usage: mv old... new")
	}

	items, _ := snap.EvalList(args)
	if len(items) < 2 {
		return newErrUsage("usage: mv old... new")
	}

	for _, item := range items[:len(items)-1] {
		if item.Kind == refactor.ItemNotFound {
			return newErrPrecondition("%s not found", item.Name)
		}
	}

	srcs, dst := items[:len(items)-1], items[len(items)-1]
	if dst.Kind == refactor.ItemDir || dst.Kind == refactor.ItemFile {
		var dstPkg *refactor.Package
		if dst.Kind == refactor.ItemDir {
			for _, pkg := range snap.Packages() {
				if pkg.PkgPath == dst.Name && pkg.Name != "" {
					dstPkg = pkg
					break
				}
			}
			if dstPkg == nil {
				p, err := snap.CreatePackage(dst.Name)
				if err != nil {
					return fmt.Errorf("mv ..., %s: %w", dst.Name, err)
				}
				dstPkg = p
			}
		} else {
			if !strings.Contains(dst.Name, "/") {
				dstPkg = snap.Target() // TODO
			} else {
				pkgPath := path.Dir(dst.Name)
				for _, pkg := range snap.Packages() {
					if pkg.PkgPath == pkgPath {
						dstPkg = pkg
						break
					}
				}
				if dstPkg == nil {
					// TODO: Load on demand.
					return newErrPrecondition("unknown package %s", dst.Name)
				}
				dst.Name = path.Base(dst.Name)
			}
		}

		for _, item := range srcs {
			if item.Kind == refactor.ItemNotFound {
				return newErrPrecondition("%s not found", item.Name)
			}
			if item.Outer == nil && item.Kind != refactor.ItemDir {
				// ok
				continue
			}
			if item.Kind == refactor.ItemMethod && item.Outer.Kind == refactor.ItemType {
				// ok
				continue
			}
			what := "directory"
			if dst.Kind == refactor.ItemFile {
				what = "file"
			}
			return newErrPrecondition("cannot move %s to %s %s", item.Kind, what, dst.Name)
		}

		mvCode(snap, srcs, dst, dstPkg)
		return nil
	}

	// Otherwise, renaming to program identifier, which must not exist.
	if len(items) != 2 {
		return newErrUsage("cannot move multiple items to %s", dst.Name)
	}

	old, newItem := items[0], items[1]

	if old.Kind == refactor.ItemNotFound {
		return newErrPrecondition("%s not found", old.Name)
	}

	if old.Kind == refactor.ItemPos && newItem.Kind == refactor.ItemPos {
		// Text move.
		text := snap.Text(old.Pos, old.End)
		snap.DeleteAt(old.Pos, old.End)
		snap.DeleteAt(newItem.Pos, newItem.End)
		snap.InsertAt(newItem.End, string(text))
		return nil
	}

	var newOuter *refactor.Item
	newPath := newItem.Name
	newPrefix, newName, ok := cutLast(newPath, ".")
	if ok {
		newOuter = snap.Eval(newPrefix)
		if newOuter == nil {
			return newErrPrecondition("cannot find destination %s", newPrefix)
		}
	} else {
		newName = newItem.Name
	}

	if old.Kind == refactor.ItemPos && newOuter == nil {
		mvStmt(snap, old, newName)
		return nil
	}

	// Check that newName is a valid identifier.
	// (Arbitrary syntax would make simple string search for dots invalid, among other problems.)
	if !isGoIdent.MatchString(newName) {
		return newErrUsage("malformed replacement: not a valid Go identifier: %s", newName)
	}

	// Rename of global.
	if old.Outer == nil && newOuter == nil {
		if newItem.Kind != refactor.ItemNotFound {
			snap.ErrorAt(newItem.Obj.Pos(), "already have %s", newName)
			return nil
		}
		rewriteDefn(snap, old, newName)
		rewriteUses(snap, old, newName, notInScope(newName))
		return nil
	}

	// Rename of local variable, struct field, or method.
	if old.Outer != nil && newOuter != nil && old.Outer.Obj == newOuter.Obj {
		if newItem.Kind != refactor.ItemNotFound { // TODO
			snap.ErrorAt(newItem.Obj.Pos(), "already have %s", newPath)
			return nil
		}
		rewriteDefn(snap, old, newName)
		rewriteUses(snap, old, newName, nil)
		return nil
	}

	// Rename var (global or function-local) to new field in global var of type struct.
	newTop := newOuter.Outermost()
	if old.Kind == refactor.ItemVar && (old.Outer == nil || old.Outer.Kind == refactor.ItemFunc && old.Outer.Outer == nil) &&
		newTop != nil && newTop.Kind == refactor.ItemVar &&
		(newOuter.Kind == refactor.ItemVar || newOuter.Kind == refactor.ItemField) {
		tvar := newOuter.Obj.(*types.Var)
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

			doc := removeDecl(snap, old)
			if newItem.Kind == refactor.ItemNotFound {
				addStructField(snap, doc, tvar.Pos(), structPos, newName, old.Obj.Type())
			}
			rewriteUses(snap, old, newPath, inScope(newTop.Name, newTop.Obj))
			return nil
		}
	}

	// TODO: Rename field in global struct var to plain global var.

	// TODO: Rename global function to method.
	if old.Outer == nil && old.Kind == refactor.ItemFunc &&
		newOuter != nil && newOuter.Kind == refactor.ItemType {
		if newItem != nil && newItem.Kind != refactor.ItemNotFound {
			snap.ErrorAt(newItem.Obj.Pos(), "already have %s", newPath)
			return nil
		}
		if _, ok := newOuter.Obj.(*types.TypeName); ok {
			funcToMethod(snap, old.Obj.(*types.Func), newName)
			return nil
		}
	}

	// Rename method to global function.
	if old.Kind == refactor.ItemMethod && old.Outer.Outer == nil && old.Outer.Kind == refactor.ItemType && newOuter == nil {
		if newItem.Kind != refactor.ItemNotFound { // TODO
			snap.ErrorAt(newItem.Obj.Pos(), "already have %s", newPath)
			return nil
		}
		methodToFunc(snap, old.Obj.(*types.Func), newName)
		return nil
	}

	if old.Outer == nil && newItem != nil && newTop != nil && newTop.Obj != nil {
		rewriteUses(snap, old, newPath, inScope(newTop.Name, newTop.Obj))
		return nil
	}

	// TODO: Rename global to variable in function.

	return fmt.Errorf("unimplemented replacement: %v -> %v . %v [%v]", old, newOuter, newName, newItem)
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
	outer := stack[1]
	switch outer.(type) {
	default:
		panic(refactor.StackTypes(stack))
	case *ast.FuncDecl, *ast.Field, *ast.AssignStmt:
		// ok
	case *ast.ValueSpec, *ast.TypeSpec:
		decl := stack[2].(*ast.GenDecl)
		if decl.Lparen == token.NoPos {
			outer = decl
		}
	}

	docPos, _ := nodeRange(snap, outer)
	docEnd := outer.Pos()
	text := snap.Text(docPos, docEnd)
	for _, m := range regexp.MustCompile(`\b`+regexp.QuoteMeta(old.Obj.Name())+`\b`).FindAllIndex(text, -1) {
		snap.ReplaceAt(docPos+token.Pos(m[0]), docPos+token.Pos(m[1]), new)
	}
	snap.ReplaceNode(id, new)
}

func rewriteUses(snap *refactor.Snapshot, old *refactor.Item, new string, checkPos posChecker) {
	fix := func(pkg *refactor.Package, file *ast.File) {
		refactor.Walk(file, func(stack []ast.Node) {
			id, ok := stack[0].(*ast.Ident)
			if !ok || pkg.TypesInfo.Uses[id] != old.Obj {
				if len(stack) > 2 {
					if as, ok := stack[1].(*ast.AssignStmt); ok && pkg.TypesInfo.Defs[id] == old.Obj && as.Tok != token.DEFINE {
						goto OK
					}
				}
				return
			}
		OK:
			if checkPos != nil {
				checkPos(snap, stack)
			}
			snap.ReplaceNode(id, new)
		})
	}
	// TODO: This should be something like
	//	snap.ForReverseDepsFiles
	// and it should load the reverse deps on demand.
	if !token.IsExported(old.Outermost().Name) {
		pkg, _ := snap.FileAt(old.Obj.Pos())
		for _, file := range pkg.Files {
			fix(pkg, file.Syntax)
		}
		return
	}
	snap.ForEachFile(fix)
}

func StackTypes(list []ast.Node) string {
	var types []reflect.Type
	for _, n := range list {
		types = append(types, reflect.TypeOf(n))
	}
	return fmt.Sprint(types)
}

// removeDecl removes the declaration of old,
// returning its former doc comments.
func removeDecl(snap *refactor.Snapshot, old *refactor.Item) string {
	stack := snap.SyntaxAt(old.Obj.Pos())
	// stack is *ast.Ident *ast.ValueSpec *ast.GenDecl
	// or maybe *ast.Ident *ast.AssignStmt
	switch stack[1].(type) {
	case *ast.ValueSpec:
		spec := stack[1].(*ast.ValueSpec)
		if len(spec.Values) > 0 {
			snap.ErrorAt(spec.Pos(), "removing declaration would drop initializer")
			return ""
		}
		decl := stack[2].(*ast.GenDecl)
		var docPos, docEnd token.Pos
		if len(decl.Specs) == 1 {
			docPos, _ = nodeRange(snap, decl)
			docEnd = decl.Pos()
		} else {
			docPos, _ = nodeRange(snap, spec)
			docEnd = spec.Pos()
		}
		removeDecls(snap, map[types.Object]bool{old.Obj: true})
		return string(snap.Text(docPos, docEnd))

	case *ast.AssignStmt:
		as := stack[1].(*ast.AssignStmt)
		if len(as.Lhs) > 1 {
			snap.ErrorAt(as.Pos(), "multiple decl not implemented")
			return ""
		}
		docPos, _ := nodeRange(snap, stack[1])
		// Delete colon and update as so other users can tell this is
		// no longer a declaration.
		snap.DeleteAt(as.TokPos, as.TokPos+1)
		as.Tok = token.ASSIGN
		return string(snap.Text(docPos, as.Pos()))
	}

	snap.ErrorAt(old.Obj.Pos(), "could not find declaration for %v to delete", old.Name)
	return ""
}

func addStructField(snap *refactor.Snapshot, doc string, oldPos, structPos token.Pos, name string, typ types.Type) {
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
		snap.InsertAt(fields.Closing, "\n")
	}
	var buf bytes.Buffer
	printType(&buf, snap, oldPos, structPos, typ)
	snap.InsertAt(fields.Closing, doc+name+" "+buf.String()+"\n")
}

func methodToFunc(snap *refactor.Snapshot, method *types.Func, name string) {
	// Convert method declaration.
	// Insert name before receiver list.
	srcPkg, _ := snap.FileAt(method.Pos())
	stack := snap.SyntaxAt(method.Pos()) // FuncType Ident FuncDecl
	decl := stack[2].(*ast.FuncDecl)
	snap.InsertAt(decl.Recv.Opening, " "+name)

	// Drop ) MethodName( from declaration, replacing with comma if there are arguments.
	sep := ""
	if len(decl.Type.Params.List) > 0 {
		sep = ", "
	}
	snap.ReplaceAt(decl.Recv.Closing, decl.Type.Params.Opening+1, sep)

	// If receiver is named but params are not, or vice versa,
	// need to add _ to each name.
	if len(decl.Type.Params.List) > 0 &&
		(len(decl.Recv.List[0].Names) == 0) != (len(decl.Type.Params.List[0].Names) == 0) {
		if len(decl.Recv.List[0].Names) == 0 {
			pos := decl.Recv.List[0].Type.Pos()
			snap.InsertAt(pos, "_ ")
		} else {
			for _, f := range decl.Type.Params.List {
				pos := f.Type.Pos()
				snap.InsertAt(pos, "_ ")
			}
		}
	}

	// Find and convert method uses.
	// But first, is this a pointer method or a value method?
	sig := method.Type().(*types.Signature)
	recvType := sig.Recv().Type()
	_, recvPtr := recvType.(*types.Pointer)

	snap.ForEachFile(func(pkg *refactor.Package, file *ast.File) {
		refactor.Walk(file, func(stack []ast.Node) {
			id, ok := stack[0].(*ast.Ident)
			if !ok || pkg.TypesInfo.Uses[id] != method {
				return
			}

			// Found a use to convert. Can we refer to the new name here?
			name := name
			if pkg == srcPkg {
				if snap.LookupAt(name, id.Pos()) != nil {
					snap.ErrorAt(id.Pos(), "%s already in scope", name)
				}
			} else {
				name = snap.NeedImport(id.Pos(), "", srcPkg.Types) + "." + name
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
				snap.ReplaceNode(sel, name)
				return
			}

			// Call of method expression?
			if selType {
				// T.M(rcvr, x) -> newname(rcvr, x).
				snap.ReplaceAt(call.Pos(), call.Lparen, name)
				if selPtr && !recvPtr { // (*T).M(rcvr, x) -> newname(*rcvr, x)
					// There are no binary operators that can yield a pointer,
					// so no possible need for parens around rcvr.
					snap.InsertAt(call.Lparen+1, "*")
				}
				return
			}

			// Call of method with real receiver.
			// Turn x.M(y) into name(x, y).
			// May need to turn x into &x or *x as well.
			snap.InsertAt(sel.X.Pos(), name+"(")
			snap.ReplaceAt(sel.X.End(), call.Lparen+1, "")
			if len(call.Args) > 0 {
				snap.InsertAt(call.Lparen+1, ", ")
			}
			if recvPtr && !selPtr {
				// There are no binary operators that can yield an addressable expression,
				// so no possible need for parens around x..
				snap.InsertAt(sel.X.Pos(), "&")
			}
			if selPtr && !recvPtr {
				// There are no binary operators that can yield a pointer,
				// so no possible need for parens around x.
				snap.InsertAt(sel.X.Pos(), "*")
			}
		})
	})
}

func funcToMethod(snap *refactor.Snapshot, method *types.Func, name string) {
	stack := snap.SyntaxAt(method.Pos()) // FuncType Ident FuncDecl
	decl := stack[2].(*ast.FuncDecl)
	fn := stack[0].(*ast.FuncType)

	if len(decl.Type.Params.List) < 1 {
		panic(fmt.Sprintf("function %q has no parameters", decl.Name.Name))
	}

	// Determine receiver name from the parameter list
	rcvrName := func() string {
		if len(decl.Type.Params.List[0].Names) > 0 {
			return decl.Type.Params.List[0].Names[0].Name
		}
		return ""
	}()

	// Determine receiver type from parameter list
	rcvrType := func() string {
		switch pt := decl.Type.Params.List[0].Type.(type) {
		default:
			panic(fmt.Sprintf("unexpected parameter type: %T", pt))
		case *ast.Ident:
			return pt.Name
		case *ast.StarExpr:
			ident, ok := pt.X.(*ast.Ident)
			if !ok {
				panic("parameter type is not an identifier")
			}
			return "*" + ident.Name
		}
	}()

	lo := fn.Func
	hi := func() token.Pos {
		if len(decl.Type.Params.List) > 1 {
			return decl.Type.Params.List[1].Pos() - 1
		}
		return decl.Type.Params.List[0].End()
	}()
	str := fmt.Sprintf("func (%s %s) %s (", rcvrName, rcvrType, name)
	snap.ReplaceAt(lo, hi, str)

	// Find and convert function uses.

	snap.ForEachFile(func(pkg *refactor.Package, file *ast.File) {
		refactor.Walk(file, func(stack []ast.Node) {
			id, ok := stack[0].(*ast.Ident)
			if !ok || pkg.TypesInfo.Uses[id] != method {
				return
			}

			assignExpr, selExpr := func() (*ast.AssignStmt, *ast.SelectorExpr) {
				if a, ok := stack[1].(*ast.AssignStmt); ok {
					return a, nil
				}
				if s, ok := stack[1].(*ast.SelectorExpr); ok {
					if a, ok := stack[2].(*ast.AssignStmt); ok {
						return a, s
					}
				}
				return nil, nil
			}()
			// Handle the assignment case:
			//   pkg.Foo -> (*pkg.T).Foo
			if assignExpr != nil && selExpr != nil && strings.HasPrefix(rcvrType, "*") {
				id, ok := selExpr.X.(*ast.Ident)
				if !ok {
					panic(fmt.Sprintf("unexpected type %T", selExpr.X))
				}
				repl := fmt.Sprintf("(*%s.%s).%s", id.String(), rcvrType[1:], name)
				snap.ReplaceNode(selExpr, repl)
				return
			}
			// Handle the assignment cases:
			//   Foo -> T.Foo
			//   Foo -> (*T).Foo
			//   pkg.Foo -> pkg.T.Foo
			if assignExpr != nil {
				fmtStr := func() string {
					if strings.HasPrefix(rcvrType, "*") {
						return "(%s).%s"
					}
					return "%s.%s"
				}()
				snap.ReplaceNode(id, fmt.Sprintf(fmtStr, rcvrType, name))
				return
			}

			// Handle normal function calls
			call := func() *ast.CallExpr {
				if call, ok := stack[1].(*ast.CallExpr); ok {
					return call
				}
				if call, ok := stack[2].(*ast.CallExpr); ok {
					return call
				}
				return nil
			}()
			if call == nil || len(call.Args) >= 1 {
				fn := func() *ast.Ident {
					switch t := call.Fun.(type) {
					case *ast.Ident:
						return t
					case *ast.SelectorExpr:
						return t.Sel
					default:
						return nil
					}
				}()
				if fn != nil && fn == id {
					lo = call.Pos()
					hi = func() token.Pos {
						if len(call.Args) > 1 {
							return call.Args[1].Pos() - 1
						}
						return call.Args[0].End()
					}()
					rcvrName := snap.Text(call.Args[0].Pos(), call.Args[0].End())
					snap.ReplaceAt(lo, hi, fmt.Sprintf("%s.%s(", rcvrName, name))
					return
				}
			}

			// Handle replacement within other function call
			if selExpr, ok := stack[1].(*ast.SelectorExpr); ok {
				id, ok := selExpr.X.(*ast.Ident)
				if !ok {
					panic(fmt.Sprintf("unexpected type %T", selExpr.X))
				}
				if strings.HasPrefix(rcvrType, "*") {
					repl := fmt.Sprintf("(*%s.%s).%s", id.String(), rcvrType[1:], name)
					snap.ReplaceNode(selExpr, repl)
					return
				}
				repl := fmt.Sprintf("%s.%s", rcvrType, name)
				snap.ReplaceNode(selExpr.Sel, repl)
				return
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
