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

func (s *Snapshot) LookupAt(name string, pos token.Pos) types.Object {
	for _, p := range s.packages {
		for _, file := range p.Files {
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
	WalkRange(n, 0, token.Pos(^uint(0)>>1), f)
}

func WalkRange(n ast.Node, lo, hi token.Pos, f func(stack []ast.Node)) {
	var stack []ast.Node
	var stackPos int

	ast.Inspect(n, func(n ast.Node) bool {
		if n == nil {
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
		f(stack[stackPos:])
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
		for _, file := range p.Files {
			syntax := file.Syntax
			filename := s.Position(syntax.Package).Filename
			if seen[filename] {
				continue
			}
			seen[filename] = true
			f(p, syntax)
		}
	}
}

func (s *Snapshot) ForEachTargetFile(f func(pkg *Package, file *ast.File)) {
	seen := make(map[string]bool)
	p := s.target
	for _, file := range p.Files {
		syntax := file.Syntax
		filename := s.Position(syntax.Package).Filename
		if seen[filename] {
			continue
		}
		seen[filename] = true
		f(p, syntax)
	}
}
