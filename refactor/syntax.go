// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package refactor

import (
	"go/ast"
	"go/token"
	"go/types"
	"path/filepath"
)

func (s *Snapshot) Position(pos token.Pos) token.Position {
	return s.fset.Position(pos)
}

func (s *Snapshot) File(pos token.Pos) string {
	return s.fset.Position(pos).Filename
}

func (s *Snapshot) Addr(pos token.Pos) string {
	p := s.Position(pos)
	p.Filename = s.r.shortPath(p.Filename)
	return p.String()
}

func (s *Snapshot) Text(lo, hi token.Pos) []byte {
	plo := s.Position(lo)
	phi := s.Position(hi)
	f := s.files[plo.Filename]
	if f == nil {
		panic("file not found")
	}
	return f.Text[plo.Offset:phi.Offset]
}

func (s *Snapshot) SyntaxAt(pos token.Pos) []ast.Node {
	_, file := s.FileAt(pos)
	if file == nil {
		return nil
	}

	var stack []ast.Node
	ast.Inspect(file, func(n ast.Node) bool {
		if n == nil || pos < n.Pos() || n.End() <= pos {
			return false
		}
		stack = append(stack, n)
		return true
	})
	for i, j := 0, len(stack)-1; i < j; i, j = i+1, j-1 {
		stack[i], stack[j] = stack[j], stack[i]
	}
	return stack
}

func (s *Snapshot) FileByName(name string) (*Package, *ast.File) {
	if !filepath.IsAbs(name) {
		name = filepath.Join(s.r.dir, name)
	}
	name = s.r.shortPath(name)

	for _, p := range s.packages {
		for _, file := range p.Files {
			if file.Name == name {
				return p, file.Syntax
			}
		}
	}
	if ed := s.edits[name]; ed != nil {
		return ed.Package, ed.File.Syntax
	}
	return nil, nil
}

func (s *Snapshot) PackageAt(pos token.Pos) *Package {
	pkg, _ := s.FileAt(pos)
	return pkg
}

func (s *Snapshot) FileRange(pos token.Pos) (start, end token.Pos) {
	tf := s.Fset().File(pos)
	start = token.Pos(tf.Base())
	return start, start + token.Pos(tf.Size())
}

func (s *Snapshot) FileAt(pos token.Pos) (*Package, *ast.File) {
	tfile := s.fset.File(pos)
	name := tfile.Name()
	f := s.files[name]

	for _, p := range s.packages {
		for _, file := range p.Files {
			if file.Syntax == nil {
				continue
			}
			tfile := s.fset.File(file.Syntax.Pos())
			if file == f {
				start := token.Pos(tfile.Base())
				end := start + token.Pos(tfile.Size())
				if !(tfile.Base() <= int(pos) && int(pos) <= tfile.Base()+tfile.Size()) {
					println("WEIRD", name, pos, start, end)
					panic("POS")
				}
				return p, f.Syntax
			}
			_ = f
			if tfile.Base() <= int(pos) && int(pos) <= tfile.Base()+tfile.Size() {
				return p, file.Syntax
			}
		}
	}

	return nil, nil
}

func (s *Snapshot) ScopeAt(pos token.Pos) *types.Scope {
	for _, p := range s.packages {
		for _, file := range p.Files {
			if file.Syntax == nil {
				continue
			}
			f := file.Syntax
			if f.Pos() <= pos && pos < f.End() {
				return p.TypesInfo.Scopes[f].Innermost(pos)
			}
		}
	}
	return nil
}

func (s *Snapshot) LookupAt(name string, pos token.Pos) types.Object {
	for _, p := range s.packages {
		for _, file := range p.Files {
			if file.Syntax == nil {
				continue
			}
			f := file.Syntax
			if f.Pos() <= pos && pos < f.End() {
				_, obj := p.TypesInfo.Scopes[f].Innermost(pos).LookupParent(name, pos)
				return obj
			}
		}
	}
	return nil
}

func Walk(n ast.Node, f func(stack []ast.Node)) {
	walkRange(n, 0, token.Pos(^uint(0)>>1), true, f)
}

func WalkPost(n ast.Node, f func(stack []ast.Node)) {
	walkRange(n, 0, token.Pos(^uint(0)>>1), false, f)
}

func WalkRange(n ast.Node, lo, hi token.Pos, f func(stack []ast.Node)) {
	walkRange(n, lo, hi, true, f)
}

func WalkRangePost(n ast.Node, lo, hi token.Pos, f func(stack []ast.Node)) {
	walkRange(n, lo, hi, false, f)
}

func walkRange(n ast.Node, lo, hi token.Pos, preorder bool, f func(stack []ast.Node)) {
	var stack []ast.Node
	var stackPos int

	if n == nil || n == (*ast.File)(nil) || n == (*ast.BlockStmt)(nil) {
		// Happens for deleted files, where f.Syntax == nil.
		// Easier to catch here than fix every call site.
		return
	}

	ast.Inspect(n, func(n ast.Node) bool {
		if n == nil {
			if !preorder {
				f(stack[stackPos:])
			}
			stackPos++
			return true
		}
		if n.End() < lo || hi <= n.Pos() {
			return false
		}
		if stackPos == 0 {
			old := len(stack)
			stack = append(stack, nil)
			stack = stack[:cap(stack)]
			copy(stack[len(stack)-old:], stack[:old])
			stackPos = len(stack) - old
		}
		stackPos--
		stack[stackPos] = n
		if preorder {
			f(stack[stackPos:])
		}
		return true
	})

	if stackPos != len(stack) {
		panic("internal stack error")
	}
}

func (s *Snapshot) ReplaceNode(n ast.Node, repl string) {
	s.ReplaceAt(n.Pos(), n.End(), repl)
}

func (s *Snapshot) ForEachFile(f func(pkg *Package, file *ast.File)) {
	seen := make(map[string]bool)
	for _, p := range s.packages {
		if p.TypesInfo == nil {
			// Package newly created.
			continue
		}
		for _, file := range p.Files {
			if seen[file.Name] || file.Syntax == nil {
				continue
			}
			seen[file.Name] = true
			f(p, file.Syntax)
		}
	}
}

func (s *Snapshot) ForEachTargetFile(f func(pkg *Package, file *ast.File)) {
	p := s.target
	for _, file := range p.Files {
		if file.Syntax != nil {
			f(p, file.Syntax)
		}
	}
}
