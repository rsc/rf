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
	s   *Snapshot
	pos token.Pos // position of old[0]
	end token.Pos // pos+len(old)
	old []byte
	q   edits
}

// An edit records a single text modification: change the bytes in [start,end) to new.
type edit struct {
	pos   token.Pos
	end   token.Pos
	new   string
	force bool
}

// An edits is a list of edits that is sortable by start offset, breaking ties by end offset.
type edits []edit

func (x edits) Len() int      { return len(x) }
func (x edits) Swap(i, j int) { x[i], x[j] = x[j], x[i] }
func (x edits) Less(i, j int) bool {
	// Earlier edits first.
	if x[i].pos != x[j].pos {
		return x[i].pos < x[j].pos
	}
	// Force delete before other delete/replace.
	if x[i].force != x[j].force {
		return x[i].force
	}
	return false
}

func NewBufferAt(s *Snapshot, pos token.Pos, text []byte) *Buffer {
	return &Buffer{s: s, pos: pos, end: pos + token.Pos(len(text)), old: text}
}

func (b *Buffer) edit(pos, end token.Pos, new string, force bool) {
	if end < pos || pos < 0 || end > b.end {
		panic("invalid edit position")
	}
	b.q = append(b.q, edit{pos: pos, end: end, new: new, force: force})
}

func (b *Buffer) Delete(pos, end token.Pos)              { b.edit(pos, end, "", false) }
func (b *Buffer) ForceDelete(pos, end token.Pos)         { b.edit(pos, end, "", true) }
func (b *Buffer) Insert(pos token.Pos, new string)       { b.edit(pos, pos, new, false) }
func (b *Buffer) Replace(pos, end token.Pos, new string) { b.edit(pos, end, new, false) }

// Bytes returns a new byte slice containing the original data
// with the queued edits applied.
func (b *Buffer) Bytes() []byte {
	// Sort edits by starting position and then by ending position.
	// Breaking ties by ending position allows insertions at point x
	// to be applied before a replacement of the text at [x, y).
	// Given multiple inserts at the same position, keep in order.
	sort.Stable(b.q)

	var new []byte
	offset := b.pos
	for i := 0; i < len(b.q); i++ {
		e := b.q[i]
		if e.pos < offset {
			e0 := b.q[i-1]
			panic(fmt.Sprintf("%v: overlapping edits: [%d,%d)->%q, [%d,%d)->%q", b.s.Addr(e0.pos), e0.pos, e0.end, e0.new, e.pos, e.end, e.new))
		}
		new = append(new, b.old[offset-b.pos:e.pos-b.pos]...)
		if e.force {
			for i+1 < len(b.q) && !b.q[i+1].force && b.q[i+1].pos < e.end {
				i++
			}
		}
		offset = e.end
		new = append(new, e.new...)
	}
	new = append(new, b.old[offset-b.pos:]...)
	return new
}

// String returns a string containing the original data
// with the queued edits applied.
func (b *Buffer) String() string {
	return string(b.Bytes())
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
	b := NewBufferAt(s, pos-token.Pos(posn.Offset), f.Text)
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

func (s *Snapshot) CreateFile(p *Package, baseName, text string) *ast.File {
	name := filepath.Join(p.Dir, baseName)
	// TODO
	if text == "" {
		text = "package " + p.Name + "\n"
	}
	base := s.fset.Base()

	file, err := s.r.cache.newFileText(name, []byte(text), true)
	if err != nil {
		println("TEXT", text)
		panic("CreateFile parse: " + err.Error())
	}
	ed := &Edit{
		Name:   name,
		Create: true,
		Buffer: NewBufferAt(s, token.Pos(base), []byte(text)),
		File:   file,
	}
	if s.files[file.Name] != nil || s.edits[file.Name] != nil {
		panic(fmt.Sprintf("file %s created twice", file.Name))
	}
	s.edits[file.Name] = ed

	p.Files = append(p.Files, ed.File)
	sort.Slice(p.Files, func(i, j int) bool {
		return p.Files[i].Name < p.Files[j].Name
	})
	return file.Syntax
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
		files, _ := ioutil.ReadDir(dir)
		for _, file := range files {
			if strings.HasSuffix(file.Name(), ".go") {
				return nil, fmt.Errorf("%s exists but not loaded", pkgpath)
			}
		}
	}

	p := &Package{
		Name:            path.Base(pkgpath),
		Dir:             dir,
		ID:              pkgpath,
		PkgPath:         pkgpath,
		InCurrentModule: true,
	}
	p.Types = types.NewPackage(p.PkgPath, p.Name)
	s.pkgGraph.add(p)
	s.packages = append(s.packages, p)
	return p, nil
}

// currentBytes returns the contents of the given file after any edits in s.
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
	if f.Deleted {
		return []byte{}
	}
	return f.Text
}

// oldBytes returns the original text of the given file, prior to any edits.
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

// fileNames returns the names of all files in Snapshot, including those created
// by Edits.
func (s *Snapshot) fileNames() []string {
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

	return names
}

func (s *Snapshot) Diff() ([]byte, error) {
	var diffs []byte
	for _, name := range s.fileNames() {
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
	created := make(map[string]int)
	failed := false
	for _, name := range s.fileNames() {
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
			text := deleteUnusedImports(s, p, ed.Buffer.Bytes())
			file, err := parser.ParseFile(fset, "out.go", text, parser.ParseComments)
			if err != nil {
				continue
			}
			var out bytes.Buffer
			if err := format.Node(&out, fset, file); err != nil {
				continue
			}
			ed.Buffer = NewBufferAt(s, ^token.Pos(0), out.Bytes())
		}
	}
}
