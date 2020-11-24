// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package refactor

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/format"
	"go/parser"
	"go/token"
	"go/types"
	"io/ioutil"
	"os"
	"path"
	"path/filepath"
	"sort"
	"strings"

	"rsc.io/rf/diff"
	"rsc.io/rf/edit"
)

type Edit struct {
	Name       string
	OldText    []byte
	Create     bool
	Delete     bool
	Buffer     *Buffer
	Package    *Package
	File       *File
	AddImports []NewImport
}

type NewImport struct {
	id  string
	pkg *types.Package
}

func (e *Edit) NewText() ([]byte, error) {
	b := e.Buffer.Bytes()
	return b, nil
}

// A Buffer is a queue of edits to apply to a file text.
// It's like edit.Buffer but uses token.Pos as coordinate space.
type Buffer struct {
	pos token.Pos
	end token.Pos
	ed  *edit.Buffer
}

func NewBufferAt(pos token.Pos, text []byte) *Buffer {
	return &Buffer{pos: pos, end: pos + token.Pos(len(text)), ed: edit.NewBuffer(text)}
}

func (b *Buffer) Bytes() []byte {
	return b.ed.Bytes()
}

func (b *Buffer) String() string {
	return b.ed.String()
}

func (b *Buffer) Delete(pos, end token.Pos) {
	b.ed.Delete(int(pos-b.pos), int(end-b.pos))
}

func (b *Buffer) ForceDelete(pos, end token.Pos) {
	b.ed.ForceDelete(int(pos-b.pos), int(end-b.pos))
}

func (b *Buffer) Insert(pos token.Pos, new string) {
	b.ed.Insert(int(pos-b.pos), new)
}

func (b *Buffer) Replace(pos, end token.Pos, new string) {
	b.ed.Replace(int(pos-b.pos), int(end-b.pos), new)
}

func (s *Snapshot) editAt(pos token.Pos) *Edit {
	posn := s.Position(pos)
	name := posn.Filename
	ed := s.edits[name]
	if ed != nil {
		return ed
	}
	f := s.files[name]
	if f == nil {
		panic("file not found")
	}
	b := NewBufferAt(pos-token.Pos(posn.Offset), f.Text)
	ed = &Edit{Name: name, OldText: f.Text, Buffer: b}
	s.edits[name] = ed
	return ed
}

func (s *Snapshot) bufferAt(pos token.Pos) *Buffer {
	return s.editAt(pos).Buffer
}

func (s *Snapshot) ReplaceAt(lo, hi token.Pos, repl string) {
	b := s.bufferAt(lo)
	if b == nil {
		// File is being deleted
		return
	}
	b.Replace(lo, hi, repl)
}

func (s *Snapshot) InsertAt(pos token.Pos, repl string) {
	s.ReplaceAt(pos, pos, repl)
}

func (s *Snapshot) DeleteAt(pos, end token.Pos) {
	s.ReplaceAt(pos, end, "")
}

func (s *Snapshot) ForceDeleteAt(pos, end token.Pos) {
	s.bufferAt(pos).ForceDelete(pos, end)
}

func (s *Snapshot) DeleteFile(pos token.Pos) {
	name := s.Position(pos).Filename
	s.edits[name] = &Edit{Name: name, Delete: true}
}

func (s *Snapshot) CreateFile(p *Package, name, text string) *ast.File {
	name = s.r.shortPath(filepath.Join(p.Dir, name))
	// TODO
	if text == "" {
		text = "package " + p.Name + "\n"
	}
	base := s.fset.Base()
	syntax, err := parser.ParseFile(s.fset, name, text, parser.ParseComments)
	if err != nil {
		println("TEXT", text)
		panic("CreateFile parse: " + err.Error())
	}
	ed := &Edit{
		Name:   name,
		Create: true,
		Buffer: NewBufferAt(token.Pos(base), []byte(text)),
		File:   &File{Name: name, Syntax: syntax},
	}
	s.edits[name] = ed

	// TODO: what about adding to package?
	p.Files = append(p.Files, ed.File)
	sort.Slice(p.Files, func(i, j int) bool {
		return p.Files[i].Name < p.Files[j].Name
	})
	s.files[name] = ed.File
	return syntax
}

func (s *Snapshot) CreatePackage(pkgpath string) (*Package, error) {

	if pkgpath == "." || strings.HasPrefix(pkgpath, "./") || strings.HasPrefix(pkgpath, "../") {
		pkgpath = path.Join(s.target.PkgPath, pkgpath)
	}

	dir, err := s.r.PkgDir(pkgpath)
	if err != nil {
		return nil, err
	}

	if _, err := os.Stat(dir); err == nil {
		return nil, fmt.Errorf("%s exists but not loaded", pkgpath)
	}

	p := &Package{
		Name:            path.Base(pkgpath),
		Dir:             dir,
		ID:              pkgpath,
		PkgPath:         pkgpath,
		InCurrentModule: true,
	}
	p.Types = types.NewPackage(p.PkgPath, p.Name)
	s.pkgByID[p.ID] = p
	s.packages = append(s.packages, p)
	return p, nil
}

func (s *Snapshot) currentBytes(name string) []byte {
	ed := s.edits[name]
	if ed != nil {
		if ed.Delete {
			return []byte{}
		}
		return ed.Buffer.Bytes()
	}
	f := s.files[name]
	if f == nil {
		return nil
	}
	return f.Text
}

func (s *Snapshot) oldBytes(name string) []byte {
	for s.parent != nil {
		s = s.parent
	}
	f := s.files[name]
	if f == nil {
		return nil
	}
	return f.Text
}

func (s *Snapshot) Diff() ([]byte, error) {
	var names []string
	for name := range s.files {
		names = append(names, name)
	}

	sort.Slice(names, func(i, j int) bool {
		di, dj := filepath.Dir(names[i]), filepath.Dir(names[j])
		if di != dj {
			return di < dj
		}
		return names[i] < names[j]
	})

	var diffs []byte
	for _, name := range names {
		new := s.currentBytes(name)
		if new == nil {
			continue
		}
		old := s.oldBytes(name)
		if bytes.Equal(old, new) {
			continue
		}
		if !filepath.IsAbs(name) {
			name = filepath.Join(s.r.dir, name)
		}
		rel, err := filepath.Rel(s.r.modRoot, name)
		if err != nil {
			panic(err)
		}
		d, err := diff.Diff("old/"+rel, old, "new/"+rel, new)
		if err != nil {
			return nil, err
		}
		diffs = append(diffs, d...)
	}
	return diffs, nil
}

func (s *Snapshot) Write() error {
	var names []string
	for name := range s.files {
		names = append(names, name)
	}
	for name, ed := range s.edits {
		if ed.Create {
			names = append(names, name)
		}
	}
	sort.Slice(names, func(i, j int) bool {
		di, dj := filepath.Dir(names[i]), filepath.Dir(names[j])
		if di != dj {
			return di < dj
		}
		return names[i] < names[j]
	})

	created := make(map[string]int)
	failed := false
	for _, name := range names {
		new := s.currentBytes(name)
		old := s.oldBytes(name)
		if bytes.Equal(old, new) {
			continue
		}
		var err error
		if len(new) == 0 {
			err = os.Remove(name)
		} else {
			dir := filepath.Dir(name)
			if created[dir] == 0 {
				created[dir] = 1
				if info, err := os.Stat(dir); err != nil || !info.IsDir() {
					if err := os.MkdirAll(dir, 0777); err != nil {
						fmt.Fprintf(s.r.Stderr, "%s\n", err)
						failed = true
						created[dir] = 2
						continue
					}
				}
			}
			if created[dir] == 1 {
				err = ioutil.WriteFile(name, new, 0666)
			}
		}
		if err != nil {
			fmt.Fprintf(s.r.Stderr, "%s\n", err)
			failed = true
		}
	}
	if failed {
		return fmt.Errorf("errors writing files")
	}
	return nil
}

func (s *Snapshot) Modified() []string {
	seen := make(map[string]bool)
	var paths []string
	for _, p := range s.packages {
		path := strings.TrimSuffix(p.PkgPath, "_test")
		if seen[path] {
			continue
		}
		for _, f := range p.Files {
			if s.edits[f.Name] != nil {
				seen[path] = true
				paths = append(paths, path)
				break
			}
		}
	}
	return paths
}

func (s *Snapshot) Gofmt() {
	s.addImports()
	for _, p := range s.packages {
		for _, f := range p.Files {
			ed := s.edits[f.Name]
			if ed == nil || ed.Delete {
				continue
			}
			fset := token.NewFileSet()
			file, err := parser.ParseFile(fset, "out.go", ed.Buffer.Bytes(), parser.ParseComments)
			if err != nil {
				continue
			}
			deleteUnusedImports(s, p, file)

			var out bytes.Buffer
			if err := format.Node(&out, fset, file); err != nil {
				continue
			}
			ed.Buffer = NewBufferAt(^token.Pos(0), out.Bytes())
		}
	}
}
