// Copyright 2014 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Adapted from eg.

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
	verbose bool
}

// match reports whether pattern x matches y.
func (m *matcher) match(x, y ast.Node) bool {
	if len(m.env) > 0 {
		for k := range m.env {
			delete(m.env, k)
		}
	}
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
	x = unparen(x)
	y = unparen(y)

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
	return types.Identical(tx, ty)
}

func (m *matcher) wildcardObj(x ast.Expr) (*types.Var, bool) {
	if x, ok := x.(*ast.Ident); ok && x != nil && m.wildOK {
		if xobj, ok := m.infoX.Uses[x].(*types.Var); ok && xobj.Pos() >= m.wildPos {
			return xobj, true
		}
	}
	return nil, false
}

func (m *matcher) matchSelectorExpr(x, y *ast.SelectorExpr) bool {
	if xobj, ok := m.wildcardObj(x.X); ok {
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
	}
	return m.matchExpr(x.X, y.X)
}

func (m *matcher) matchWildcard(xobj *types.Var, y ast.Expr) bool {
	name := xobj.Name()

	if m.verbose {
		fmt.Fprintf(os.Stderr, "%s: wildcard %s -> %s?: ",
			m.fset.Position(y.Pos()), name, astString(m.fset, y))
	}

	// Check that y is assignable to the declared type of the param.
	yt := typeOf(m.infoY, y)
	if yt == nil {
		// y has no type.
		// Perhaps it is an *ast.Ellipsis in [...]T{}, or
		// an *ast.KeyValueExpr in T{k: v}.
		// Clearly these pseudo-expressions cannot match a
		// wildcard, but it would nice if we had a way to ignore
		// the difference between T{v} and T{k:v} for structs.
		return false
	}
	if !types.AssignableTo(yt, xobj.Type()) {
		if m.verbose {
			fmt.Fprintf(os.Stderr, "%s not assignable to %s\n", yt, xobj.Type())
		}
		return false
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
