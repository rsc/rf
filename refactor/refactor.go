// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package refactor

import (
	"encoding/json"
	"fmt"
	"go/ast"
	"go/format"
	"go/parser"
	"go/token"
	"go/types"
	"io"
	"io/ioutil"
	"log"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"

	"golang.org/x/tools/go/packages"
	"rsc.io/rf/diff"
	"rsc.io/rf/edit"
)

// A Refactor holds the state for an active refactoring.
type Refactor struct {
	dir     string
	self    *packages.Package
	g       *Snapshot
	modRoot string
	modPath string
}

// New returns a new refactoring,
// editing the package in the given directory (usually ".").
func New(dir, pkg string) (*Refactor, error) {
	dir, err := filepath.EvalSymlinks(dir)
	if err != nil {
		return nil, err
	}
	dir = filepath.Clean(dir)

	cfg := &packages.Config{
		Mode:  packages.NeedName | packages.NeedModule,
		Dir:   dir,
		Tests: true, // in case this is a test-only package
	}
	pkgs, err := packages.Load(cfg, pkg)
	if err != nil {
		return nil, err
	}
	if len(pkgs) == 0 {
		return nil, fmt.Errorf("cannot load " + pkg + " in " + dir)
	}

	// TODO: pkgs[0].Module should work, but go list -json cmd/compile
	// doesn't show a Module.
	/*
		m := pkgs[0].Module
		if m == nil {
			return nil, fmt.Errorf("cannot find module for " + dir)
		}
		if !m.Main {
			return nil, fmt.Errorf("cannot refactor module in module cache")
		}
		modRoot := m.Dir
		modPath := m.Path
	*/

	cmd := exec.Command("go", "env", "GOMOD")
	cmd.Dir = dir
	bmod, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("loading module: %v", err)
	}
	mod := strings.TrimSpace(string(bmod))
	if filepath.Base(mod) != "go.mod" {
		return nil, fmt.Errorf("no module found for " + dir)
	}
	modRoot := filepath.Dir(mod)

	cmd = exec.Command("go", "mod", "edit", "-json")
	cmd.Dir = dir
	js, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("loading module: %v", err)
	}
	var d struct{ Module struct{ Path string } }
	if err := json.Unmarshal(js, &d); err != nil {
		return nil, fmt.Errorf("loading module: %v", err)
	}
	modPath := d.Module.Path

	r := &Refactor{
		dir:     dir,
		self:    pkgs[0],
		modRoot: modRoot,
		modPath: modPath,
	}
	return r, nil
}

func (r *Refactor) ModPath() string {
	return r.modPath
}

func (r *Refactor) ModRoot() string {
	return r.modRoot
}

func (r *Refactor) PkgPath() string {
	return r.self.PkgPath
}

func (r *Refactor) Importers() ([]string, error) {
	cfg := &packages.Config{
		Mode:  packages.NeedName | packages.NeedImports,
		Dir:   r.modRoot,
		Tests: true,
	}

	// Load all the packages in the module that might import the target.
	// If the refactoring target is an internal package,
	// we only need to consider other packages that can see it,
	// not everything in the module.
	pattern := "./..."
	if strings.Contains(r.self.PkgPath, "/internal/") {
		if prefix, _, _ := cut(r.self.PkgPath, "/internal/"); prefix != r.modPath {
			pattern = "./" + prefix[len(r.modPath)+1:] + "/..."
		}
	}
	pkgs, err := packages.Load(cfg, pattern)
	if err != nil {
		return nil, err
	}

	// Filter results down to those that do import the target.
	seen := map[string]bool{r.self.PkgPath: true}
	var paths []string
	for _, pkg := range pkgs {
		if pkg.Imports[r.self.PkgPath] == nil {
			continue
		}
		path := pkg.PkgPath
		switch {
		case strings.HasSuffix(path, ".test"):
			// Test main - we don't care.
			continue
		case strings.HasSuffix(path, "_test"):
			// External test package - include it.
			path = strings.TrimSuffix(path, "_test")
		}
		if !seen[path] {
			seen[path] = true
			paths = append(paths, path)
		}
	}

	return paths, nil
}

// A Snapshot is a collection of loaded packages
// and pending edits.
type Snapshot struct {
	r     *Refactor
	fset  *token.FileSet
	files fileCache
	stale bool
	pkgs  []*Package
	edits map[string]*edit.Buffer
}

func (s *Snapshot) Packages() []*Package {
	return s.pkgs
}

type Package struct {
	g   *Snapshot
	Pkg *packages.Package
}

type fileCache struct {
	mu   sync.Mutex
	data map[string][]byte
}

func (fc *fileCache) cacheRead(name string, src []byte) []byte {
	fc.mu.Lock()
	if fc.data[name] == nil {
		if fc.data == nil {
			fc.data = make(map[string][]byte)
		}
		fc.data[name] = src
	} else {
		src = fc.data[name]
	}
	fc.mu.Unlock()
	return src
}

func (fc *fileCache) ParseFile(fset *token.FileSet, filename string, src []byte) (*ast.File, error) {
	const mode = parser.AllErrors | parser.ParseComments
	return parser.ParseFile(fset, filename, fc.cacheRead(filename, src), mode)
}

func (r *Refactor) Load(extra ...string) (*Snapshot, error) {
	return r.load(0, extra)
}

func (r *Refactor) LoadTyped(extra ...string) (*Snapshot, error) {
	return r.load(packages.NeedTypes|packages.NeedTypesInfo, extra)
}

func (r *Refactor) load(mode packages.LoadMode, extra []string) (*Snapshot, error) {
	g := &Snapshot{
		r:     r,
		fset:  token.NewFileSet(),
		edits: make(map[string]*edit.Buffer),
	}
	if r.g != nil {
		for name, edit := range r.g.edits {
			g.files.cacheRead(name, edit.Bytes())
		}
	}
	cfg := &packages.Config{
		Mode:      packages.NeedName | packages.NeedSyntax | mode,
		Dir:       r.dir,
		Tests:     true,
		Fset:      g.fset,
		ParseFile: g.files.ParseFile,
	}
	pkgs, err := packages.Load(cfg, append([]string{"."}, extra...)...)
	if err != nil {
		return nil, err
	}
	rpkgs := []*Package{nil, nil}
	var self, selfTest *Package
	for _, p := range pkgs {
		if strings.HasSuffix(p.ID, ".test") {
			continue
		}
		rp := &Package{Pkg: p, g: g}
		if p.PkgPath == r.self.PkgPath {
			if p.ID == p.PkgPath {
				// Actual package
				if self != nil {
					return nil, fmt.Errorf("duplicate results for target")
				}
				self = rp
				continue
			} else {
				// Package built with tests.
				if selfTest != nil {
					return nil, fmt.Errorf("duplicate results for target test")
				}
				selfTest = rp
				continue
			}
		}
		rpkgs = append(rpkgs, rp)
	}
	if selfTest != nil && self != nil {
		rpkgs[0] = selfTest
		rpkgs[1] = self
	} else if selfTest != nil {
		rpkgs[1] = selfTest
		rpkgs = rpkgs[1:]
	} else if self != nil {
		rpkgs[1] = self
		rpkgs = rpkgs[1:]
	} else {
		return nil, fmt.Errorf("did not find target")
	}
	g.pkgs = append(g.pkgs, rpkgs...)
	if r.g != nil {
		r.g.stale = true
	}
	r.g = g
	return g, nil
}

func (p *Package) File(pos token.Pos) string {
	return p.g.fset.Position(pos).Filename
}

// shortPath returns an absolute or relative name for path, whatever is shorter.
func (r *Refactor) shortPath(path string) string {
	if rel, err := filepath.Rel(r.dir, path); err == nil && len(rel) < len(path) {
		return rel
	}
	return path
}

func (p *Package) Addr(pos token.Pos) string {
	p1 := p.g.fset.Position(pos)
	p1.Filename = p.g.r.shortPath(p1.Filename)
	return p1.String()
}

func (p *Package) Position(pos token.Pos) token.Position {
	return p.g.fset.Position(pos)
}

func (p *Package) Fset() *token.FileSet {
	return p.g.fset
}

func (p *Package) Text(lo, hi token.Pos) []byte {
	plo := p.Position(lo)
	phi := p.Position(hi)
	text := p.g.files.cacheRead(plo.Filename, nil)
	if text == nil {
		return nil
	}
	return text[plo.Offset:phi.Offset]
}

func (p *Package) Edit(file string) *edit.Buffer {
	if p.g.edits[file] == nil {
		text := p.g.files.cacheRead(file, nil)
		if text != nil {
			p.g.edits[file] = edit.NewBuffer(text)
		}
	}
	return p.g.edits[file]
}

func (s *Snapshot) Diff() ([]byte, error) {
	var diffs []byte
	for _, p := range s.pkgs {
		d, err := p.Diff()
		if err != nil {
			return nil, err
		}
		diffs = append(diffs, d...)
	}
	return diffs, nil
}

func (p *Package) Diff() ([]byte, error) {
	var diffs []byte
	for _, file := range p.Pkg.Syntax {
		name := p.File(file.Package)
		buf, ok := p.g.edits[name]
		if !ok {
			continue
		}
		rel, err := filepath.Rel(p.g.r.modRoot, name)
		d, err := diff.Diff("old/"+rel, p.g.files.cacheRead(name, nil), "new/"+rel, buf.Bytes())
		if err != nil {
			return nil, err
		}
		diffs = append(diffs, d...)
	}
	return diffs, nil
}

func (s *Snapshot) Modified() []string {
	seen := make(map[string]bool)
	var paths []string
	for _, p := range s.pkgs {
		path := strings.TrimSuffix(p.Pkg.PkgPath, "_test")
		if seen[path] {
			continue
		}
		for _, file := range p.Pkg.Syntax {
			name := p.File(file.Package)
			if _, ok := p.g.edits[name]; ok {
				seen[path] = true
				paths = append(paths, path)
				break
			}
		}
	}
	return paths
}

func (s *Snapshot) Gofmt() {
	for _, p := range s.pkgs {
		for _, file := range p.Pkg.Syntax {
			name := p.File(file.Package)
			buf, ok := p.g.edits[name]
			if !ok {
				continue
			}
			text, err := format.Source(buf.Bytes())
			if err == nil {
				p.g.edits[name] = edit.NewBuffer(text)
			}
		}
	}
}

func (s *Snapshot) Write(stderr io.Writer) error {
	failed := false
	for _, p := range s.pkgs {
		for _, file := range p.Pkg.Syntax {
			name := p.File(file.Package)
			buf, ok := p.g.edits[name]
			if !ok {
				continue
			}
			if err := ioutil.WriteFile(name, buf.Bytes(), 0666); err != nil {
				fmt.Fprintf(stderr, "%s\n", err)
				failed = true
			}
		}
	}
	if failed {
		return fmt.Errorf("errors writing files")
	}
	return nil
}

func (p *Package) LookupAt(name string, pos token.Pos) types.Object {
	for _, file := range p.Pkg.Syntax {
		if file.Pos() <= pos && pos < file.End() {
			_, obj := p.Pkg.TypesInfo.Scopes[file].Innermost(pos).LookupParent(name, pos)
			return obj
		}
	}
	return nil
}

func PrintErrors(w io.Writer, snap *Snapshot) bool {
	found := false
	for _, p := range snap.Packages() {
		if len(p.Pkg.Errors) > 0 {
			for _, e := range p.Pkg.Errors {
				if file, rest, ok := cut(e.Pos, ":"); ok {
					e.Pos = p.g.r.shortPath(file) + ":" + rest
				}
				fmt.Fprintln(w, e)
				found = true
			}
		}
	}
	return found
}

type Item struct {
	Kind  ItemKind
	Name  string
	Obj   types.Object
	Outer *Item
}

type ItemKind int

const (
	_ ItemKind = iota
	ItemFile
	ItemDir
	ItemConst
	ItemType
	ItemVar
	ItemFunc
	ItemField
	ItemMethod
)

func (k ItemKind) String() string {
	switch k {
	case ItemFile:
		return "file"
	case ItemDir:
		return "dir"
	case ItemConst:
		return "const"
	case ItemType:
		return "type"
	case ItemVar:
		return "var"
	case ItemFunc:
		return "func"
	case ItemField:
		return "field"
	case ItemMethod:
		return "method"
	}
	return "???"
}

func (p *Package) Lookup(expr string) *Item {
	// Special cases for directory, file, as in "mv Thing ../newpkg".
	if strings.Contains(expr, "/") {
		return &Item{Kind: ItemDir, Name: expr}
	}
	if strings.HasSuffix(expr, ".go") {
		return &Item{Kind: ItemFile, Name: expr}
	}

	name, rest, more := cut(expr, ".")
	item := lookupTop(p, name)
	if item == nil {
		return nil
	}
	item.Name = name
	for more {
		name, rest, more = cut(rest, ".")
		item = lookupIn(p, item, name)
		if item == nil {
			return nil
		}
		item.Name = item.Outer.Name + "." + name
	}
	return item
}

func lookupTop(p *Package, expr string) *Item {
	obj := p.Pkg.Types.Scope().Lookup(expr)
	switch obj := obj.(type) {
	default:
		log.Fatalf("%s is a %T, unimplemented", expr, obj)
		return nil
	case nil:
		return nil
	case *types.TypeName:
		return &Item{Kind: ItemType, Obj: obj}
	case *types.Const:
		return &Item{Kind: ItemConst, Obj: obj}
	case *types.Var:
		return &Item{Kind: ItemVar, Obj: obj}
	case *types.Func:
		return &Item{Kind: ItemFunc, Obj: obj}
	}
}

func lookupIn(p *Package, outer *Item, name string) *Item {
	switch outer.Kind {
	case ItemConst:
		return nil
	case ItemType:
		// Look for method, field.
		return lookupType(p, outer, outer.Obj.Type(), name)
	case ItemVar:
		// If unnamed struct or interface, look in type.
		switch typ := outer.Obj.Type().Underlying().(type) {
		case *types.Struct, *types.Interface:
			return lookupType(p, outer, typ, name)
		}
	case ItemFunc:
		// Look for declaration inside function.
	case ItemField:
		return lookupType(p, outer, outer.Obj.Type(), name)
	}
	return nil
}

func lookupType(p *Package, outer *Item, typ types.Type, name string) *Item {
	if tn, ok := typ.(*types.Named); ok {
		n := tn.NumMethods()
		for i := 0; i < n; i++ {
			f := tn.Method(i)
			if f.Name() == name {
				return &Item{Kind: ItemMethod, Obj: f, Outer: outer}
			}
		}
		typ = tn.Underlying()
	}

	if typ, ok := typ.(*types.Struct); ok {
		n := typ.NumFields()
		for i := 0; i < n; i++ {
			f := typ.Field(i)
			if f.Name() == name {
				return &Item{Kind: ItemField, Obj: f, Outer: outer}
			}
		}
	}
	return nil
}

func cut(s, sep string) (before, after string, ok bool) {
	if i := strings.Index(s, sep); i >= 0 {
		return s[:i], s[i+len(sep):], true
	}
	return s, "", false
}

func cutLast(s, sep string) (before, after string, ok bool) {
	if i := strings.LastIndex(s, sep); i >= 0 {
		return s[:i], s[i+len(sep):], true
	}
	return s, "", false
}

type ErrorPrinter interface {
	ErrorAt(pos token.Pos, format string, args ...interface{})
	NumErrors() int
}

type Rewriter interface {
	Rewrite(pkg *Package, file *ast.File, ep ErrorPrinter)
}

func NewErrorPrinter(pkg *Package, w io.Writer) ErrorPrinter {
	return &errorPrinter{pkg: pkg, w: w}
}

type errorPrinter struct {
	pkg *Package
	w   io.Writer
	n   int
}

func (ep *errorPrinter) ErrorAt(pos token.Pos, format string, args ...interface{}) {
	msg := fmt.Sprintf(format, args...)
	msg = strings.TrimRight(msg, "\n")
	msg = strings.Replace(msg, "\n", "\n\t", -1)
	fmt.Fprintf(ep.w, "%s: %s\n", ep.pkg.Addr(pos), msg)
	ep.n++
}

func (ep *errorPrinter) NumErrors() int {
	return ep.n
}

func Apply(rw Rewriter, pkgs []*Package, ep ErrorPrinter) {
	// Rewrite the target package.
	seen := make(map[string]bool)
	p := pkgs[0]
	for _, file := range p.Pkg.Syntax {
		seen[p.File(file.Package)] = true
		rw.Rewrite(p, file, ep)
	}

	// If the target package couldn't be converted, don't bother about importers.
	if ep.NumErrors() > 0 {
		return
	}

	// Make changes in other packages too.
	// Note that some source files may appear in multiple packages
	// due to compiling test packages and non-test packages.
	// Proess each only once.
	for _, p := range pkgs {
		for _, file := range p.Pkg.Syntax {
			name := p.File(file.Package)
			if seen[name] {
				continue
			}
			seen[name] = true
			rw.Rewrite(p, file, ep)
		}
	}
}
