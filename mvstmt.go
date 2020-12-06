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
	"strings"

	"rsc.io/rf/refactor"
)

func mvStmt(snap *refactor.Snapshot, old *refactor.Item, name string) {
	stack := snap.SyntaxAt(old.Pos)

	// Walk up the stack to find the first syntactic unit that
	// extends beyond the designated area.
	var inner ast.Node
	for len(stack) > 0 && old.Pos <= stack[0].Pos() && stack[0].End() <= old.End {
		inner, stack = stack[0], stack[1:]
	}
	_ = inner
	if len(stack) == 0 {
		snap.ErrorAt(old.Pos, "cannot find code to move")
		return
	}

	m := &mvStmts{
		snap: snap,
		old:  old,
		name: name,
	}
	for i := 0; i < len(stack); i++ {
		if fn, ok := stack[i].(*ast.FuncDecl); ok {
			m.fn = fn
			break
		}
	}
	if m.fn == nil {
		snap.ErrorAt(old.Pos, "code to move is not in function")
		return
	}

	m.outside = stack[0] // syntax element immediately outside (above in tree) code moving
	switch x := m.outside.(type) {
	case *ast.BlockStmt:
		m.list = x.List
	case *ast.CaseClause:
		m.list = x.Body
	case ast.Expr:
		m.expr = inner.(ast.Expr)
	case ast.Stmt:
		if inner, ok := inner.(ast.Expr); ok {
			m.expr = inner
		} else {
			m.list = []ast.Stmt{x}
			m.outside = stack[1]
		}
	}

	if m.expr == nil {
		list := m.list
		for len(list) > 0 && list[0].End() < old.Pos {
			list = list[1:]
		}
		for len(list) > 0 && list[len(list)-1].Pos() >= old.End {
			list = list[:len(list)-1]
		}
		if len(list) == 0 {
			snap.ErrorAt(old.Pos, "cannot find code to move")
			return
		}
		if old.Pos > list[0].Pos() || old.End < list[len(list)-1].End() {
			snap.ErrorAt(old.Pos, "code to move is not a list of statements [%v %v] [%v %v] [%v %v]", snap.Addr(old.Pos), snap.Addr(old.End), snap.Addr(list[0].Pos()), snap.Addr(list[len(list)-1].End()), snap.Addr(m.outside.Pos()), snap.Addr(m.outside.End()))
			return
		}
		m.list = list
	}

	m.findRefs()
	m.findControlFlow()

	if m.expr != nil {
		if len(m.results) > 0 {
			snap.ErrorAt(old.Pos, "cannot move expression writing to %v", m.results)
			return
		}
	}

	m.moveCode()
}

type mvStmts struct {
	snap       *refactor.Snapshot
	old        *refactor.Item
	name       string
	list       []ast.Stmt
	outside    ast.Node
	expr       ast.Expr
	fn         *ast.FuncDecl
	srcPkg     *refactor.Package
	srcFile    *ast.File
	returnStmt string
	params     []types.Object
	results    []types.Object
	cflow      string
	rets       []ast.Node
	needReturn bool
}

// findRefs finds which variables are referenced by the moving code
// and determines which must be passed in and which must be returned.
// It leaves the two lists in m.params and m.results.
func (m *mvStmts) findRefs() {
	m.srcPkg, m.srcFile = m.snap.FileAt(m.old.Pos)
	didParam := make(map[types.Object]bool)
	didResult := make(map[types.Object]bool)
	wrote := make(map[types.Object]bool)
	refactor.Walk(m.fn, func(stack []ast.Node) {
		if _, ok := stack[0].(*ast.ReturnStmt); ok {
			// Treat as writing all the named results.
			if m.fn.Type.Results != nil {
				for _, f := range m.fn.Type.Results.List {
					for _, id := range f.Names {
						wrote[m.srcPkg.TypesInfo.Defs[id]] = true
					}
				}
			}
			return
		}
		id, ok := stack[0].(*ast.Ident)
		if !ok {
			return
		}
		if id.End() < m.old.Pos {
			return
		}
		obj := m.srcPkg.TypesInfo.Uses[id]
		if obj == nil || obj.Parent() == types.Universe || obj.Parent() == m.srcPkg.Types.Scope() {
			return
		}
		if _, ok := obj.(*types.PkgName); ok {
			return
		}
		if sel, ok := stack[1].(*ast.SelectorExpr); ok && id == sel.Sel {
			return
		}

		if obj.Pos() < m.old.Pos && id.End() < m.old.End {
			if !didParam[obj] {
				didParam[obj] = true
				m.params = append(m.params, obj)
			}
		}
		if id.End() < m.old.End {
			if as, ok := stack[1].(*ast.AssignStmt); ok && as.Tok == token.ASSIGN {
				for _, n := range as.Lhs {
					if nid, ok := n.(*ast.Ident); ok {
						wrote[m.srcPkg.TypesInfo.Uses[nid]] = true
					}
				}
			}
		}
		if (m.old.Pos <= obj.Pos() && obj.Pos() < m.old.End || wrote[obj]) && m.old.End < id.Pos() {
			if !didResult[obj] {
				didResult[obj] = true
				m.results = append(m.results, obj)
			}
		}
	})
}

// moveCode does the actual moving of the code.
func (m *mvStmts) moveCode() {
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "return")
	for i, obj := range m.results {
		if i > 0 {
			fmt.Fprintf(&buf, ",")
		}
		fmt.Fprintf(&buf, " %s", obj.Name())
	}
	m.returnStmt = buf.String()

	buf.Reset()
	fmtType := func(typ types.Type) {
		printType(&buf, m.snap, m.old.Pos, m.srcFile.End(), typ)
	}
	fmt.Fprintf(&buf, "\n\nfunc %s(", m.name)
	for i, obj := range m.params {
		if i > 0 {
			fmt.Fprintf(&buf, ", ")
		}
		fmt.Fprintf(&buf, "%s ", obj.Name())
		fmtType(obj.Type())
	}
	fmt.Fprintf(&buf, ")")
	if m.expr != nil {
		fmt.Fprintf(&buf, " ")
		fmtType(m.srcPkg.TypesInfo.TypeOf(m.expr))
	} else if m.cflow == "return" {
		if res := m.fn.Type.Results; res != nil {
			// TODO(rsc): This is wrong, in that it should declare needed names
			// but not names already duplicated in the parameters.
			// For now, it's fine.
			fmt.Fprintf(&buf, "%s", m.snap.Text(res.Pos(), res.End()))
		}
	} else if len(m.results) > 0 {
		if len(m.results) == 1 {
			fmt.Fprintf(&buf, " ")
			fmtType(m.results[0].Type())
		} else {
			fmt.Fprintf(&buf, " (")
			for i, obj := range m.results {
				if i > 0 {
					fmt.Fprintf(&buf, ", ")
				}
				if len(m.results) > 1 {
					// Print names to make the result order clear.
					fmt.Fprintf(&buf, "_%s ", obj.Name())
				}
				fmtType(obj.Type())
			}
			fmt.Fprintf(&buf, ")")
		}
	}
	fmt.Fprintf(&buf, " {\n")
	if m.expr != nil {
		fmt.Fprintf(&buf, "\treturn ")
	}
	ed := refactor.NewBufferAt(m.snap, m.old.Pos, m.snap.Text(m.old.Pos, m.old.End))
	for _, ret := range m.rets {
		if ret != nil {
			ed.Replace(ret.Pos(), ret.End(), m.returnStmt)
		}
	}
	buf.Write(ed.Bytes())
	trimBlankLines(&buf)
	if len(m.results) > 0 && m.needReturn {
		fmt.Fprintf(&buf, "\t%s\n", m.returnStmt)
	}
	fmt.Fprintf(&buf, "}\n\n")

	m.snap.InsertAt(m.fn.End(), buf.String())

	buf.Reset()
	if m.cflow == "return" {
		if m.fn.Type.Results != nil {
			fmt.Fprintf(&buf, "return ")
		}
	} else {
		for i, obj := range m.results {
			if i > 0 {
				fmt.Fprintf(&buf, ", ")
			}
			fmt.Fprintf(&buf, "%s", obj.Name())
		}
		if len(m.results) > 0 {
			fmt.Fprintf(&buf, " := ")
		}
	}
	fmt.Fprintf(&buf, "%s(", m.name)
	for i, obj := range m.params {
		if i > 0 {
			fmt.Fprintf(&buf, ", ")
		}
		fmt.Fprintf(&buf, "%s", obj.Name())
	}
	fmt.Fprintf(&buf, ")")
	if m.expr == nil {
		fmt.Fprintf(&buf, "\n")
	}
	if m.cflow != "" && m.cflow != "return" && !strings.HasPrefix(m.cflow, "implicit ") {
		fmt.Fprintf(&buf, "\t%s\n", m.cflow)
	}

	m.snap.ReplaceAt(m.old.Pos, m.old.End, buf.String())
}

func (m *mvStmts) findControlFlow() {
	if m.list == nil {
		return
	}

	// Determine what kind of control flow branches are present in the code.
	// For now, we require that all the exits from the code do the same thing,
	// so that that thing can be inserted at the extraction location as well.

	localLabels := make(map[string]bool)
	refactor.WalkRange(m.srcFile, m.list[0].Pos(), m.list[len(m.list)-1].End(), func(stack []ast.Node) {
		if lab, ok := stack[0].(*ast.LabeledStmt); ok {
			localLabels[lab.Label.Name] = true
		}
	})

	var cflow []string
	var cpos []token.Pos
	cflows := make(map[string]bool)
	add := func(stmt ast.Node, s string) {
		var pos token.Pos
		if stmt == nil {
			pos = m.list[len(m.list)-1].End() + 1 // try to skip over \n or ;
		} else {
			pos = stmt.Pos()
		}
		if !cflows[s] {
			cflows[s] = true
			cflow = append(cflow, s)
			cpos = append(cpos, pos)
		}
		m.rets = append(m.rets, stmt)
	}

	refactor.WalkRange(m.srcFile, m.list[0].Pos(), m.list[len(m.list)-1].End(), func(stack []ast.Node) {
		if ret, ok := stack[0].(*ast.ReturnStmt); ok {
			add(ret, "return")
			return
		}
		br, ok := stack[0].(*ast.BranchStmt)
		if !ok {
			return
		}
		switch br.Tok {
		case token.GOTO:
			if !localLabels[br.Label.Name] {
				add(br, "goto "+br.Label.Name)
			}

		case token.FALLTHROUGH:
			outside := false
			for i := 0; i < len(stack); i++ {
				outside = outside || stack[i] == m.outside
				if _, ok := stack[i].(*ast.CaseClause); ok {
					if outside {
						add(br, "fallthrough")
					}
					return
				}
			}
			m.snap.ErrorAt(br.Pos(), "cannot find target for fallthrough")

		case token.BREAK:
			if br.Label != nil {
				if !localLabels[br.Label.Name] {
					add(br, "break "+br.Label.Name)
				}
				return
			}

			// Search for unlabeled target.
			outside := false
			for i := 0; i < len(stack); i++ {
				outside = outside || stack[i] == m.outside
				switch stack[i].(type) {
				case *ast.CaseClause, *ast.ForStmt, *ast.RangeStmt:
					if outside {
						add(br, "break")
					}
					return
				}
			}
			m.snap.ErrorAt(br.Pos(), "cannot find target for break")

		case token.CONTINUE:
			if br.Label != nil {
				if !localLabels[br.Label.Name] {
					add(br, "continue "+br.Label.Name)
				}
				return
			}

			// Search for unlabeled target.
			outside := false
			for i := 0; i < len(stack); i++ {
				outside = outside || stack[i] == m.outside
				switch stack[i].(type) {
				case *ast.ForStmt, *ast.RangeStmt:
					if outside {
						add(br, "continue")
					}
					return
				}
			}
			m.snap.ErrorAt(br.Pos(), "cannot find target for continue")
		}
	})

	implicit := false
	last := m.list[len(m.list)-1]
	if !isTerm(last, "") {
		m.needReturn = true
		// Code runs off end of list.
		// Special case implicit break in case statement and implicit return at end of function.
		switch out := m.outside.(type) {
		case *ast.CaseClause:
			if len(out.Body) > 0 && out.Body[len(out.Body)-1] == last {
				implicit = true
				if len(cflow) != 1 || cflow[0] != "break" {
					add(nil, "implicit break") // +1 to try to get past ; or \n
				}
			}
		case *ast.BlockStmt:
			if out == m.fn.Body && len(out.List) > 0 && out.List[len(out.List)-1] == last {
				implicit = true
				if len(cflow) != 1 || cflow[0] != "return" {
					add(nil, "implicit return") // +1 to try to get past ; or \n
				}
			}
		}
		if !implicit {
			add(nil, "implicit next statement") // +1 to try to get past ; or \n
		}
	}

	if len(cflow) > 1 {
		var buf bytes.Buffer
		for i := range cflow {
			fmt.Fprintf(&buf, "\n%s: %s", m.snap.Addr(cpos[i]), cflow[i])
		}
		m.snap.ErrorAt(m.old.Pos, "multiple control flow exits from moved code:%s", buf.String())
		return
	}
	if len(cflow) == 0 {
		return
	}

	m.cflow = cflow[0]
	if m.cflow == "return" {
		// Keep return statements unmodified.
		// Call site will return them up.
		m.results = nil
		m.rets = nil
	}
}

func isTerm(last ast.Stmt, label string) bool {
	switch s := last.(type) {
	case *ast.LabeledStmt:
		return isTerm(s.Stmt, s.Label.Name)
	case *ast.BlockStmt:
		return len(s.List) > 0 && isTerm(s.List[len(s.List)-1], "")
	case *ast.BranchStmt:
		return true
	case *ast.IfStmt:
		return isTerm(s.Body, "") && s.Else != nil && isTerm(s.Else, "")
	case *ast.ForStmt:
		return s.Cond == nil && !hasBreak(s.Body, 0, label)
	case *ast.ReturnStmt:
		return true
	case *ast.SelectStmt:
		for _, stmt := range s.Body.List {
			c := stmt.(*ast.CommClause)
			if len(c.Body) == 0 || !isTerm(c.Body[len(c.Body)-1], "") || hasBreakList(c.Body, 0, label) {
				return false
			}
		}
		return true
	case *ast.SwitchStmt:
		sawDef := false
		for _, stmt := range s.Body.List {
			c := stmt.(*ast.CaseClause)
			if len(c.Body) == 0 || !isTerm(c.Body[len(c.Body)-1], "") || hasBreakList(c.Body, 0, label) {
				return false
			}
			if len(c.List) == 0 {
				sawDef = true
			}
		}
		if !sawDef {
			return false
		}
		return true
	case *ast.TypeSwitchStmt:
		sawDef := false
		for _, stmt := range s.Body.List {
			c := stmt.(*ast.CaseClause)
			if len(c.Body) == 0 || !isTerm(c.Body[len(c.Body)-1], "") || hasBreakList(c.Body, 0, label) {
				return false
			}
			if len(c.List) == 0 {
				sawDef = true
			}
		}
		if !sawDef {
			return false
		}
		return true
	}
	return false
}

func hasBreakList(list []ast.Stmt, depth int, label string) bool {
	for _, s := range list {
		if hasBreak(s, depth, label) {
			return true
		}
	}
	return false
}

func hasBreak(s ast.Stmt, depth int, label string) bool {
	switch s := s.(type) {
	case *ast.BlockStmt:
		return hasBreakList(s.List, depth, label)
	case *ast.BranchStmt:
		if s.Tok == token.BREAK {
			if s.Label == nil && depth == 0 || s.Label != nil && s.Label.Name == label {
				return true
			}
		}
	case *ast.ForStmt:
		return hasBreak(s.Body, depth+1, label)
	case *ast.IfStmt:
		return hasBreak(s.Body, depth, label) || s.Else != nil && hasBreak(s.Else, depth, label)
	case *ast.LabeledStmt:
		return hasBreak(s.Stmt, depth, label)
	case *ast.RangeStmt:
		return hasBreak(s.Body, depth+1, label)
	case *ast.SelectStmt:
		for _, stmt := range s.Body.List {
			c := stmt.(*ast.CommClause)
			if hasBreakList(c.Body, depth+1, label) {
				return true
			}
		}
	case *ast.SwitchStmt:
		for _, stmt := range s.Body.List {
			c := stmt.(*ast.CaseClause)
			if hasBreakList(c.Body, depth+1, label) {
				return true
			}
		}
	case *ast.TypeSwitchStmt:
		for _, stmt := range s.Body.List {
			c := stmt.(*ast.CaseClause)
			if hasBreakList(c.Body, depth+1, label) {
				return true
			}
		}
	}
	return false
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

func trimBlankLines(buf *bytes.Buffer) {
	data := buf.Bytes()
	for len(data) > 0 && (data[len(data)-1] == '\n' || data[len(data)-1] == ' ' || data[len(data)-1] == '\t') {
		data = data[:len(data)-1]
	}
	buf.Truncate(len(data))
	buf.WriteByte('\n')
}
