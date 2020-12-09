// Copyright 2014 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Expression matching adapted from eg;
// type matching adapted from go/types.

package main

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/constant"
	"go/printer"
	"go/token"
	"go/types"
	"log"
	"os"
	"reflect"
)

type matcher struct {
	fset    *token.FileSet
	wildOK  bool
	wildPos token.Pos
	pkgX    *types.Package
	pkgY    *types.Package
	infoX   *types.Info
	infoY   *types.Info
	env     map[string]ast.Expr
	envT    map[string]types.Type
	stricts map[types.Object]bool
	verbose bool
}

func (m *matcher) reset() {
	if len(m.env) > 0 {
		for k := range m.env {
			delete(m.env, k)
		}
	}
	if len(m.envT) > 0 {
		for k := range m.envT {
			delete(m.envT, k)
		}
	}
}

// match reports whether pattern x matches y.
func (m *matcher) match(x, y ast.Node) bool {
	if x == nil && y == nil {
		return true
	}
	if x == nil || y == nil {
		return false
	}

	if x, ok := x.(ast.Stmt); ok {
		if y, ok := y.(ast.Stmt); ok {
			return m.matchStmt(x, y)
		}
	}
	if x, ok := x.(ast.Expr); ok {
		if y, ok := y.(ast.Expr); ok {
			return m.matchExpr(x, y)
		}
	}
	return false
}

func (m *matcher) matchStmt(x, y ast.Stmt) bool {
	if reflect.TypeOf(x) != reflect.TypeOf(y) {
		return false
	}
	switch x := x.(type) {
	case *ast.AssignStmt:
		y := y.(*ast.AssignStmt)
		if len(x.Lhs) != len(y.Lhs) || len(x.Rhs) != len(y.Rhs) || x.Tok != y.Tok {
			return false
		}
		for i := range x.Lhs {
			if !m.matchExpr(x.Lhs[i], y.Lhs[i]) {
				return false
			}
		}
		for i := range x.Rhs {
			if !m.matchExpr(x.Rhs[i], y.Rhs[i]) {
				return false
			}
		}
		return true

	case *ast.IncDecStmt:
		y := y.(*ast.IncDecStmt)
		return x.Tok == y.Tok && m.matchExpr(x.X, y.X)

	case *ast.SendStmt:
		y := y.(*ast.SendStmt)
		return m.matchExpr(x.Chan, y.Chan) && m.matchExpr(x.Value, y.Value)
	}
	return false
}

// matchExpr reports whether pattern x matches y.
//
// If m.wildOK, Idents in x that refer to parameters (Pos >= wildPos) are
// treated as wildcards, and match any y that is assignable to the
// parameter type; matchExpr records this correspondence in m.env.
// Otherwise, matchExpr simply reports whether the two trees are
// equivalent.
//
// A wildcard appearing more than once in the pattern must
// consistently match the same tree.
//
func (m *matcher) matchExpr(x, y ast.Expr) bool {
	if x == nil || y == nil {
		return x == y
	}

	x = unparen(x)
	y = unparen(y)

	xtv := m.infoX.Types[x]
	ytv := m.infoY.Types[y]
	if xtv.IsType() != ytv.IsType() || xtv.IsValue() != ytv.IsValue() {
		return false
	}

	if xtv.IsType() {
		return m.identical(xtv.Type, ytv.Type)
	}

	// Is x a wildcard?  (a reference to a 'before' parameter)
	if xobj, ok := m.wildcardObj(x); ok {
		return m.matchWildcard(xobj, y)
	}

	// Object identifiers (including pkg-qualified ones)
	// are handled semantically, not syntactically.
	xobj := isRef(x, m.infoX)
	yobj := isRef(y, m.infoY)
	if xobj != nil {
		return xobj == yobj
	}
	if yobj != nil {
		return false
	}

	// TODO(adonovan): audit: we cannot assume these ast.Exprs
	// contain non-nil pointers.  e.g. ImportSpec.Name may be a
	// nil *ast.Ident.

	if reflect.TypeOf(x) != reflect.TypeOf(y) {
		return false
	}
	switch x := x.(type) {
	default:
		panic(fmt.Sprintf("unhandled AST node type: %T", x))

	case *ast.Ident:
		log.Fatalf("unexpected Ident: %s", astString(m.fset, x))
		panic("unreachable")

	case *ast.BasicLit:
		y := y.(*ast.BasicLit)
		xval := constant.MakeFromLiteral(x.Value, x.Kind, 0)
		yval := constant.MakeFromLiteral(y.Value, y.Kind, 0)
		return constant.Compare(xval, token.EQL, yval)

	case *ast.FuncLit:
		// func literals (and thus statement syntax) never match.
		return false

	case *ast.CompositeLit:
		y := y.(*ast.CompositeLit)
		return (x.Type == nil) == (y.Type == nil) &&
			(x.Type == nil || m.matchType(x.Type, y.Type)) &&
			m.matchExprs(x.Elts, y.Elts)

	case *ast.SelectorExpr:
		y := y.(*ast.SelectorExpr)
		return m.matchSelectorExpr(x, y) &&
			m.infoX.Selections[x].Obj() == m.infoY.Selections[y].Obj()

	case *ast.IndexExpr:
		y := y.(*ast.IndexExpr)
		return m.matchExpr(x.X, y.X) &&
			m.matchExpr(x.Index, y.Index)

	case *ast.SliceExpr:
		y := y.(*ast.SliceExpr)
		return m.matchExpr(x.X, y.X) &&
			m.matchExpr(x.Low, y.Low) &&
			m.matchExpr(x.High, y.High) &&
			m.matchExpr(x.Max, y.Max) &&
			x.Slice3 == y.Slice3

	case *ast.TypeAssertExpr:
		y := y.(*ast.TypeAssertExpr)
		return m.matchExpr(x.X, y.X) &&
			m.matchType(x.Type, y.Type)

	case *ast.CallExpr:
		y := y.(*ast.CallExpr)
		match := m.matchExpr // function call
		if m.infoX.Types[x.Fun].IsType() {
			match = m.matchType // type conversion
		}
		return x.Ellipsis.IsValid() == y.Ellipsis.IsValid() &&
			match(x.Fun, y.Fun) &&
			m.matchExprs(x.Args, y.Args)

	case *ast.StarExpr:
		y := y.(*ast.StarExpr)
		return m.matchExpr(x.X, y.X)

	case *ast.UnaryExpr:
		y := y.(*ast.UnaryExpr)
		return x.Op == y.Op &&
			m.matchExpr(x.X, y.X)

	case *ast.BinaryExpr:
		y := y.(*ast.BinaryExpr)
		return x.Op == y.Op &&
			m.matchExpr(x.X, y.X) &&
			m.matchExpr(x.Y, y.Y)

	case *ast.KeyValueExpr:
		y := y.(*ast.KeyValueExpr)
		return m.matchExpr(x.Key, y.Key) &&
			m.matchExpr(x.Value, y.Value)
	}
}

func (m *matcher) matchExprs(xx, yy []ast.Expr) bool {
	if len(xx) != len(yy) {
		return false
	}
	for i := range xx {
		if !m.matchExpr(xx[i], yy[i]) {
			return false
		}
	}
	return true
}

// typeOf returns the type of x, which must be an expression (not a type).
// Otherwise after 'var i int', int itself matches i.
func typeOf(info *types.Info, x ast.Expr) types.Type {
	tv := info.Types[x]
	if tv.IsType() {
		return nil
	}
	return tv.Type
}

// matchType reports whether the two type ASTs denote identical types.
func (m *matcher) matchType(x, y ast.Expr) bool {
	tx := m.infoX.Types[x].Type
	ty := m.infoY.Types[y].Type
	return m.identical(tx, ty)
}

func (m *matcher) wildcardObj(x ast.Expr) (types.Object, bool) {
	if x, ok := x.(*ast.Ident); ok && x != nil && m.wildOK {
		if xobj, ok := m.infoX.Uses[x]; ok && xobj.Pos() >= m.wildPos {
			switch xobj.(type) {
			case *types.TypeName, *types.Var:
				return xobj, true
			}
		}
	}
	return nil, false
}

func (m *matcher) matchSelectorExpr(x, y *ast.SelectorExpr) bool {
	if xobj, ok := m.wildcardObj(x.X); ok {
		switch xobj := xobj.(type) {
		case *types.Var:
			field := x.Sel.Name
			yt := typeOf(m.infoX, y.X)
			o, _, _ := types.LookupFieldOrMethod(yt, true, m.pkgY, field)
			if o == nil {
				o, _, _ = types.LookupFieldOrMethod(yt, true, m.pkgX, field)
			}
			if o != nil {
				m.env[xobj.Name()] = y.X // record binding
				return true
			}
		case *types.TypeName:
			panic("TODO: does this happen?")
		default:
			panic("unreachable")
		}
	}
	return m.matchExpr(x.X, y.X)
}

func (m *matcher) matchWildcard(xobj types.Object, y ast.Expr) bool {
	name := xobj.Name()

	if m.verbose {
		fmt.Fprintf(os.Stderr, "%s: wildcard %s -> %s?: ",
			m.fset.Position(y.Pos()), name, astString(m.fset, y))
	}

	// Check that y is assignable to the declared type of the param.
	ytv := m.infoY.Types[y]
	yt := ytv.Type
	switch xobj := xobj.(type) {
	default:
		panic("unreachable")
	case *types.TypeName:
		if !ytv.IsType() {
			return false
		}

		return m.assignableTo(yt, xobj.Type())

	case *types.Var:
		if !ytv.IsValue() {
			return false
		}
	}

	if yt == nil {
		// TODO(mdempsky): I think this should be impossible now
		// thanks to the IsValue check above.
		panic("unreachable?")

		// y has no type.
		// Perhaps it is an *ast.Ellipsis in [...]T{}, or
		// an *ast.KeyValueExpr in T{k: v}.
		// Clearly these pseudo-expressions cannot match a
		// wildcard, but it would nice if we had a way to ignore
		// the difference between T{v} and T{k:v} for structs.
		return false
	}

	if m.stricts[xobj] {
		if !m.identical(yt, xobj.Type()) {
			if m.verbose {
				fmt.Fprintf(os.Stderr, "%s not identical to %s\n", yt, xobj.Type())
			}
			return false
		}
	} else {
		if !m.assignableTo(yt, xobj.Type()) {
			if m.verbose {
				fmt.Fprintf(os.Stderr, "%s not assignable to %s\n", yt, xobj.Type())
			}
			return false
		}
	}

	// A wildcard matches any expression.
	// If it appears multiple times in the pattern, it must match
	// the same expression each time.
	// TODO(rsc): This doesn't look like the right unification logic.
	if old, ok := m.env[name]; ok {
		// found existing binding
		m.wildOK = false
		r := m.matchExpr(old, y)
		if m.verbose {
			fmt.Fprintf(os.Stderr, "%t secondary match, primary was %s\n",
				r, astString(m.fset, old))
		}
		m.wildOK = true
		return r
	}

	if m.verbose {
		fmt.Fprintf(os.Stderr, "primary match\n")
	}

	m.env[name] = y // record binding
	return true
}

// assignableTo reports whether a value of type V is assignable to a variable of type T.
// It's like types.AssignableTo, except it supports type parameters.
func (m *matcher) assignableTo(V, T types.Type) bool {
	// This code is based on go/types, but reordered somewhat
	// to avoid multiple calls to identical, as we'd then need
	// to handle saving and restoring the environment to avoid
	// partial failed matches from preventing later full matches.

	Vu := V.Underlying()
	Tu := T.Underlying()

	if v, ok := Vu.(*types.Basic); ok && v.Info()&types.IsUntyped != 0 {
		if m.isWildcardType(T) {
			panic(fmt.Sprintf("assignableTo untyped: %v -> %v", V, T))
		}
		return types.AssignableTo(Vu, Tu)
	}
	// Vu is typed

	if m.wildOK {
		if m.isWildcardType(T) {
			return m.matchWildcardType(T.(*types.Named), V)
		}
	}

	// T is an interface type and x implements T
	if Ti, ok := Tu.(*types.Interface); ok {
		return m.implements(V, Ti)
	}

	if isNamed(V) && isNamed(T) {
		return m.identical(V, T)
	}

	// x is a bidirectional channel value, T is a channel
	// type, x's type V and T have identical element types,
	// and at least one of V or T is not a named type
	if Vc, ok := Vu.(*types.Chan); ok && Vc.Dir() == types.SendRecv {
		if Tc, ok := Tu.(*types.Chan); ok {
			return m.identical(Vc.Elem(), Tc.Elem())
		}
	}

	return m.identical(Vu, Tu)
}

func isNamed(t types.Type) bool {
	switch t.(type) {
	case *types.Basic, *types.Named:
		return true
	}
	return false
}

func (m *matcher) isWildcardType(t types.Type) bool {
	name, ok := t.(*types.Named)
	return ok && name.Obj().Pos() >= m.wildPos
}

// identical reports whether x and y are identical types.
func (m *matcher) identical(x, y types.Type) bool {
	if x == y {
		return true
	}

	if m.wildOK {
		if m.isWildcardType(x) {
			return m.matchWildcardType(x.(*types.Named), y)
		}
		if m.isWildcardType(y) {
			return m.matchWildcardType(y.(*types.Named), x)
		}
	}

	if reflect.TypeOf(x) != reflect.TypeOf(y) {
		return false
	}
	switch x := x.(type) {
	case *types.Basic:
		y := y.(*types.Basic)
		return x.Kind() == y.Kind()

	case *types.Array:
		y := y.(*types.Array)
		return x.Len() == y.Len() &&
			m.identical(x.Elem(), y.Elem())

	case *types.Slice:
		y := y.(*types.Slice)
		return m.identical(x.Elem(), y.Elem())

	case *types.Struct:
		y := y.(*types.Struct)
		if x.NumFields() != y.NumFields() {
			return false
		}
		for i, n := 0, x.NumFields(); i < n; i++ {
			xf := x.Field(i)
			yf := y.Field(i)
			if xf.Embedded() != yf.Embedded() ||
				x.Tag(i) != y.Tag(i) ||
				xf.Id() != yf.Id() ||
				!m.identical(xf.Type(), yf.Type()) {
				return false
			}
		}
		return true

	case *types.Pointer:
		y := y.(*types.Pointer)
		return m.identical(x.Elem(), y.Elem())

	case *types.Tuple:
		y := y.(*types.Tuple)
		if x.Len() != y.Len() {
			return false
		}
		for i, n := 0, x.Len(); i < n; i++ {
			if !m.identical(x.At(i).Type(), y.At(i).Type()) {
				return false
			}
		}
		return true

	case *types.Signature:
		y := y.(*types.Signature)
		return x.Variadic() == y.Variadic() &&
			m.identical(x.Params(), y.Params()) &&
			m.identical(x.Results(), y.Results())

	case *types.Interface:
		y := y.(*types.Interface)
		if x.NumMethods() != y.NumMethods() {
			return false
		}
		for i, n := 0, x.NumMethods(); i < n; i++ {
			xm := x.Method(i)
			ym := y.Method(i)
			if xm.Id() != ym.Id() ||
				!m.identical(xm.Type(), ym.Type()) {
				return false
			}
		}
		return true

	case *types.Map:
		y := y.(*types.Map)
		return m.identical(x.Key(), y.Key()) &&
			m.identical(x.Elem(), y.Elem())

	case *types.Chan:
		y := y.(*types.Chan)
		return x.Dir() == y.Dir() &&
			m.identical(x.Elem(), y.Elem())

	case *types.Named:
		y := y.(*types.Named)
		return x.Obj() == y.Obj()
	}

	panic("unreachable")
}

func (m *matcher) implements(V types.Type, T *types.Interface) bool {
	if T.Empty() {
		return true
	}

	if V, ok := V.Underlying().(*types.Interface); ok {
		j := 0
		for i, n := 0, V.NumMethods(); i < n; i++ {
			if Vm, Tm := V.Method(i), T.Method(j); Vm.Id() == Tm.Id() {
				if !m.identical(Vm.Type(), Tm.Type()) {
					return false
				}
				j++
				if j == T.NumMethods() {
					return true
				}
			}
		}
		return false
	}

	// A concrete type V implements T if it implements all methods of T.
	for i, n := 0, T.NumMethods(); i < n; i++ {
		Tm := T.Method(i)

		obj, _, _ := types.LookupFieldOrMethod(V, false, Tm.Pkg(), Tm.Name())
		if Vm, ok := obj.(*types.Func); !ok || !m.identical(Vm.Type(), Tm.Type()) {
			return false
		}
	}
	return true
}

func (m *matcher) matchWildcardType(xname *types.Named, y types.Type) bool {
	if m.isAny(xname.Obj()) {
		return true
	}

	name := xname.Obj().Name()

	// A wildcard matches any expression.
	// If it appears multiple times in the pattern, it must match
	// the same expression each time.
	// TODO(rsc): This doesn't look like the right unification logic.
	if old, ok := m.envT[name]; ok {
		// found existing binding
		m.wildOK = false
		r := m.identical(old, y)
		if m.verbose {
			fmt.Fprintf(os.Stderr, "%t secondary type match, primary was %s\n",
				r, old)
		}
		m.wildOK = true
		return r
	}

	if m.verbose {
		fmt.Fprintf(os.Stderr, "primary type match\n")
	}

	m.envT[name] = y // record binding

	// Treat basic types as though they were unnamed.
	xu := xname.Underlying()
	if _, ok := xu.(*types.Basic); ok {
		y = y.Underlying()
	}

	return m.assignableTo(y, xu)
}

// isAny reports whether obj refers to rf's builtin "any" type.
func (m *matcher) isAny(obj *types.TypeName) bool {
	return obj.Name() == "any" &&
		obj.Pos() >= m.wildPos &&
		obj.Parent().Parent().Lookup("any") == nil
}

// isRef returns the object referred to by this (possibly qualified)
// identifier, or nil if the node is not a referring identifier.
func isRef(n ast.Node, info *types.Info) types.Object {
	switch n := n.(type) {
	case *ast.Ident:
		return info.Uses[n]

	case *ast.SelectorExpr:
		if _, ok := info.Selections[n]; !ok {
			// qualified ident
			return info.Uses[n.Sel]
		}
	}
	return nil
}

func unparen(e ast.Expr) ast.Expr {
	for {
		p, ok := e.(*ast.ParenExpr)
		if !ok {
			return e
		}
		e = p.X
	}
}

func astString(fset *token.FileSet, n ast.Node) string {
	var buf bytes.Buffer
	printer.Fprint(&buf, fset, n)
	return buf.String()
}
