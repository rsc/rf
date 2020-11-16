// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package refactor

import (
	"bytes"
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
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"sync"

	"golang.org/x/tools/go/packages"
	"rsc.io/rf/diff"
	"rsc.io/rf/edit"
)

// A Refactor holds the state for an active refactoring.
type Refactor struct {
	Stdout   io.Writer
	Stderr   io.Writer
	ShowDiff bool

	dir     string
	modRoot string
	modPath string
	targets []*packages.Package
}

// New returns a new refactoring,
// editing the package in the given directory (usually ".").
func New(dir string, pkgs ...string) (*Refactor, error) {
	dir, err := filepath.Abs(dir)
	if err != nil {
		return nil, err
	}
	dir, err = filepath.EvalSymlinks(dir)
	if err != nil {
		return nil, err
	}
	dir = filepath.Clean(dir)

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

	cfg := &packages.Config{
		Mode:  packages.NeedName | packages.NeedModule,
		Dir:   dir,
		Tests: true, // in case this is a test-only package
	}
	list, err := packages.Load(cfg, pkgs...)
	if err != nil {
		return nil, err
	}
	if len(list) == 0 {
		return nil, fmt.Errorf("no matching packages")
	}

	var save []*packages.Package
	for _, pkg := range list {
		if strings.HasSuffix(pkg.ID, ".test") || strings.HasSuffix(pkg.ID, "]") {
			continue
		}
		// pkg.Module should always be non-nil,
		// but when working in $GOROOT it is not filled in for cmd
		// TODO: Need to figure out whether it's not filled in
		// for std/cmd packages even when outside std/cmd.
		if pkg.Module != nil && !pkg.Module.Main {
			return nil, fmt.Errorf("cannot refactor package %s outside current module", pkg.PkgPath)
		}
		save = append(save, pkg)
	}

	r := &Refactor{
		Stdout:  os.Stdout,
		Stderr:  os.Stderr,
		dir:     dir,
		modRoot: modRoot,
		modPath: modPath,
		targets: save,
	}
	return r, nil
}

func (r *Refactor) Importers(snap *Snapshot) ([]string, error) {
	var overlay map[string][]byte
	if snap != nil {
		overlay = snap.overlay()
	}

	cfg := &packages.Config{
		Mode:    packages.NeedName | packages.NeedImports,
		Dir:     r.modRoot,
		Tests:   true,
		Overlay: overlay,
	}

	// Build list of all the packages in the module that might
	// import the targets. If a target is an internal package,
	// its visibility is reduced. It's OK to list the same pattern
	// multiple times.
	var patterns []string
	for _, target := range r.targets {
		prefix, _, ok := cut(target.PkgPath, "/internal/")
		if !ok || prefix == r.modPath {
			patterns = []string{"./..."}
			break
		}
		patterns = append(patterns, "./"+prefix[len(r.modPath)+1:]+"/...")
	}
	pkgs, err := packages.Load(cfg, patterns...)
	if err != nil {
		return nil, err
	}

	// Filter results down to those that do import the target.
	seen := make(map[string]bool)
	for _, target := range r.targets {
		seen[target.PkgPath] = true
	}
	var paths []string
	for _, pkg := range pkgs {
		need := false
		for _, target := range r.targets {
			if pkg.Imports[target.PkgPath] != nil {
				need = true
				break
			}
		}
		if !need {
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
	r       *Refactor
	old     *Snapshot
	fset    *token.FileSet
	files   fileCache
	targets []*packages.Package
	pkgs    []*packages.Package
	edits   map[string]*Buffer
	added   map[string][]newImport
	deleted map[string]bool
	created map[string]bool
	imports map[string]*types.Package
	errors  int
}

func (s *Snapshot) overlay() map[string][]byte {
	if s == nil {
		return nil
	}
	return s.files.overlay()
}

func (s *Snapshot) ErrorAt(pos token.Pos, format string, args ...interface{}) {
	msg := fmt.Sprintf(format, args...)
	msg = strings.TrimRight(msg, "\n")
	msg = strings.Replace(msg, "\n", "\n\t", -1)
	if pos == token.NoPos {
		fmt.Fprintf(s.r.Stderr, "rf: %s\n", msg)
	} else {
		fmt.Fprintf(s.r.Stderr, "%s: %s\n", s.Addr(pos), msg)
	}
	s.errors++
}

func (s *Snapshot) Errors() int {
	return s.errors
}

func (s *Snapshot) Fset() *token.FileSet { return s.fset }

func (s *Snapshot) Targets() []*packages.Package {
	return s.targets
}

func (s *Snapshot) Packages() []*packages.Package {
	return s.pkgs
}

type fileCache struct {
	mu   sync.Mutex
	data map[string][]byte
}

func (fc *fileCache) copy(c *fileCache) {
	// Assume dst -> src lock order is OK.
	c.mu.Lock()
	defer c.mu.Unlock()
	fc.mu.Lock()
	defer fc.mu.Unlock()
	if fc.data == nil {
		fc.data = make(map[string][]byte)
	}
	for name, data := range c.data {
		fc.data[name] = data
	}
}

func (fc *fileCache) write(name string, text []byte) {
	fc.mu.Lock()
	defer fc.mu.Unlock()

	if fc.data == nil {
		fc.data = make(map[string][]byte)
	}
	fc.data[name] = text
}

func (fc *fileCache) cacheRead(name string, src []byte) []byte {
	fc.mu.Lock()
	defer fc.mu.Unlock()

	if _, ok := fc.data[name]; !ok {
		if fc.data == nil {
			fc.data = make(map[string][]byte)
		}
		fc.data[name] = src
	} else {
		src = fc.data[name]
	}
	return src
}

func (fc *fileCache) overlay() map[string][]byte {
	m := make(map[string][]byte)
	fc.mu.Lock()
	for k, v := range fc.data {
		m[k] = v
	}
	fc.mu.Unlock()
	return m
}

func (fc *fileCache) ParseFile(fset *token.FileSet, filename string, src []byte) (*ast.File, error) {
	const mode = parser.AllErrors | parser.ParseComments
	return parser.ParseFile(fset, filename, fc.cacheRead(filename, src), mode)
}

func (r *Refactor) LoadUntyped(extra ...string) (*Snapshot, error) {
	return r.load(nil, 0, extra)
}

func (r *Refactor) Load(extra ...string) (*Snapshot, error) {
	return r.load(nil, packages.NeedTypes|packages.NeedTypesInfo, extra)
}

func (s *Snapshot) Load(extra ...string) (*Snapshot, error) {
	return s.r.load(s, packages.NeedTypes|packages.NeedTypesInfo, extra)
}

func (r *Refactor) load(base *Snapshot, mode packages.LoadMode, extra []string) (*Snapshot, error) {
	s := &Snapshot{
		r:       r,
		old:     base,
		fset:    token.NewFileSet(),
		edits:   make(map[string]*Buffer),
		added:   make(map[string][]newImport),
		deleted: make(map[string]bool),
		created: make(map[string]bool),
		imports: make(map[string]*types.Package),
	}
	if base != nil {
		s.files.copy(&base.files)
		for name, edit := range base.edits {
			s.files.write(name, edit.Bytes())
		}
	}
	cfg := &packages.Config{
		Mode:      packages.NeedName | packages.NeedSyntax | mode,
		Dir:       r.dir,
		Tests:     true,
		Fset:      s.fset,
		ParseFile: s.files.ParseFile,
		Overlay:   s.overlay(),
	}
	pkgs, err := packages.Load(cfg, append([]string{"."}, extra...)...)
	if err != nil {
		return nil, err
	}

	for _, p := range pkgs {
		s.imports[p.Types.Path()] = p.Types
		for _, tp := range p.Types.Imports() {
			s.imports[tp.Path()] = tp
		}
	}

	failed := false
	for _, p := range pkgs {
		if len(p.Errors) > 0 {
			failed = true
			for _, e := range p.Errors {
				if file, rest, ok := cut(e.Pos, ":"); ok {
					e.Pos = r.shortPath(file) + ":" + rest
				}
				fmt.Fprintln(r.Stderr, e)
			}
		}
	}
	if failed {
		return nil, fmt.Errorf("errors found")
	}

	targets := make([]*packages.Package, len(r.targets))
	for _, p := range pkgs {
		if strings.HasSuffix(p.ID, ".test") || strings.HasSuffix(p.ID, "]") {
			continue
		}
		for i, t := range r.targets {
			if p.PkgPath == t.PkgPath {
				// Save if this is the package we expect,
				// and replace if it's the internal test package,
				// which has strictly more code.
				if targets[i] == nil || targets[i].ID == p.PkgPath && p.ID != p.PkgPath {
					targets[i] = p
				}
			}
		}
	}
	var missing []string
	for i, p := range targets {
		if p == nil {
			missing = append(missing, r.targets[i].PkgPath)
		}
	}
	if len(missing) > 0 {
		return nil, fmt.Errorf("missing expected packages: %v", missing)
	}

	have := make(map[*packages.Package]bool)
	for _, p := range targets {
		have[p] = true
	}

	all := targets
	for _, p := range pkgs {
		if strings.HasSuffix(p.ID, ".test") || strings.HasSuffix(p.ID, "]") {
			continue
		}
		if !have[p] {
			have[p] = true
			all = append(all, p)
		}
	}

	s.targets = targets
	s.pkgs = all
	return s, nil
}

// shortPath returns an absolute or relative name for path, whatever is shorter.
func (r *Refactor) shortPath(path string) string {
	if rel, err := filepath.Rel(r.dir, path); err == nil && len(rel) < len(path) {
		return rel
	}
	return path
}

func (s *Snapshot) Addr(pos token.Pos) string {
	p := s.fset.Position(pos)
	p.Filename = s.r.shortPath(p.Filename)
	return p.String()
}

func (s *Snapshot) Text(lo, hi token.Pos) []byte {
	plo := s.Position(lo)
	phi := s.Position(hi)
	text := s.files.cacheRead(plo.Filename, nil)
	if text == nil {
		return nil
	}
	return text[plo.Offset:phi.Offset]
}

func (s *Snapshot) Position(pos token.Pos) token.Position {
	return s.fset.Position(pos)
}

func (s *Snapshot) ReplaceAt(lo, hi token.Pos, repl string) {
	plo := s.Position(lo)
	file := plo.Filename
	if s.edits[file] == nil {
		text := s.files.cacheRead(file, nil)
		s.edits[file] = NewBufferAt(lo-token.Pos(plo.Offset), text)
	}
	s.edits[file].Replace(lo, hi, repl)
}

func (s *Snapshot) InsertAt(pos token.Pos, repl string) {
	s.ReplaceAt(pos, pos, repl)
}

func (s *Snapshot) DeleteAt(pos, end token.Pos) {
	s.ReplaceAt(pos, end, "")
}

func (s *Snapshot) DeleteFile(pos token.Pos) {
	_, file := s.FileAt(pos)

	tf := s.fset.File(pos)
	pos = token.Pos(tf.Base())
	end := pos + token.Pos(tf.Size())
	s.ForceDeleteAt(pos, end)
	s.InsertAt(pos, "package "+file.Name.Name+"\n") // dummy file to satisfy future compiles
	s.deleted[s.Position(pos).Filename] = true
}

func pkgDir(pkg *packages.Package) string {
	return filepath.Dir(pkg.Fset.Position(pkg.Syntax[0].Package).Filename)
}

func (s *Snapshot) CreateFile(pkg *packages.Package, name, text string) *ast.File {
	if text == "" {
		text = "package " + pkg.Types.Name() + "\n"
	}
	filename := filepath.Join(pkgDir(pkg), name)
	s.created[filename] = true
	s.files.write(filename, []byte(text))
	f, err := parser.ParseFile(s.fset, filename, text, parser.ParseComments)
	if err != nil {
		panic("CreateFile parse: " + err.Error())
	}
	pkg.Syntax = append(pkg.Syntax, f)
	sort.Slice(pkg.Syntax, func(i, j int) bool {
		return pkg.Fset.Position(pkg.Syntax[i].Package).Filename < pkg.Fset.Position(pkg.Syntax[j].Package).Filename
	})
	return f
}

func (s *Snapshot) ForceDeleteAt(pos, end token.Pos) {
	posn := s.Position(pos)
	file := posn.Filename
	if s.edits[file] == nil {
		text := s.files.cacheRead(file, nil)
		s.edits[file] = NewBufferAt(pos-token.Pos(posn.Offset), text)
	}
	s.edits[file].ForceDelete(pos, end)
}

func (s *Snapshot) ReplaceNode(n ast.Node, repl string) {
	s.ReplaceAt(n.Pos(), n.End(), repl)
}

func (s *Snapshot) ForEachFile(f func(pkg *packages.Package, file *ast.File)) {
	seen := make(map[string]bool)
	for _, p := range s.pkgs {
		for _, file := range p.Syntax {
			filename := s.Position(file.Package).Filename
			if seen[filename] {
				continue
			}
			seen[filename] = true
			f(p, file)
		}
	}
}

func (s *Snapshot) ForEachTargetFile(f func(pkg *packages.Package, file *ast.File)) {
	seen := make(map[string]bool)
	for _, p := range s.targets {
		for _, file := range p.Syntax {
			filename := s.Position(file.Package).Filename
			if seen[filename] {
				continue
			}
			seen[filename] = true
			f(p, file)
		}
	}
}

func (s *Snapshot) File(pos token.Pos) string {
	return s.fset.Position(pos).Filename
}

func (s *Snapshot) oldBytes(name string) []byte {
	if s.created[name] {
		return nil
	}
	if s.old != nil {
		b := s.old.oldBytes(name)
		if b != nil {
			return b
		}
	}
	return s.files.cacheRead(name, nil)
}

func (s *Snapshot) currentBytes(name string) []byte {
	if s.deleted[name] {
		return []byte{}
	}
	if buf, ok := s.edits[name]; ok {
		return buf.Bytes()
	}
	if text := s.files.cacheRead(name, nil); text != nil {
		return text
	}
	return nil
}

func (s *Snapshot) Diff() ([]byte, error) {
	var names []string
	for name := range s.files.data {
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
		rel, err := filepath.Rel(s.r.modRoot, name)
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
	for name := range s.files.data {
		names = append(names, name)
	}
	sort.Slice(names, func(i, j int) bool {
		di, dj := filepath.Dir(names[i]), filepath.Dir(names[j])
		if di != dj {
			return di < dj
		}
		return names[i] < names[j]
	})

	failed := false
	for _, name := range names {
		new := s.currentBytes(name)
		if new == nil {
			continue
		}
		old := s.oldBytes(name)
		if bytes.Equal(old, new) {
			continue
		}
		var err error
		if s.deleted[name] {
			err = os.Remove(name)
		} else {
			err = ioutil.WriteFile(name, new, 0666)
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
	for _, p := range s.pkgs {
		path := strings.TrimSuffix(p.PkgPath, "_test")
		if seen[path] {
			continue
		}
		for _, file := range p.Syntax {
			name := s.File(file.Package)
			if _, ok := s.edits[name]; ok {
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
	for _, p := range s.pkgs {
		for _, file := range p.Syntax {
			name := s.File(file.Package)
			buf, ok := s.edits[name]
			if !ok {
				continue
			}
			fset := token.NewFileSet()
			file, err := parser.ParseFile(fset, "out.go", buf.Bytes(), parser.ParseComments)
			if err != nil {
				continue
			}
			deleteUnusedImports(s, fset, file)

			var out bytes.Buffer
			if err := format.Node(&out, fset, file); err != nil {
				continue
			}
			s.edits[name] = NewBufferAt(^token.Pos(0), out.Bytes())
		}
	}
}

func (s *Snapshot) LookupAt(name string, pos token.Pos) types.Object {
	for _, p := range s.pkgs {
		for _, file := range p.Syntax {
			if file.Pos() <= pos && pos < file.End() {
				_, obj := p.TypesInfo.Scopes[file].Innermost(pos).LookupParent(name, pos)
				return obj
			}
		}
	}
	return nil
}

type Item struct {
	Kind  ItemKind
	Name  string
	Outer *Item
	Obj   types.Object
	Pos   token.Pos
	End   token.Pos
}

func (i *Item) Outermost() *Item {
	for i != nil && i.Outer != nil {
		i = i.Outer
	}
	return i
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
	ItemPos
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
	case ItemPos:
		return "text"
	}
	return "???"
}

func (s *Snapshot) Lookup(expr string) *Item {
	// Special cases for directory, file, as in "mv Thing ../newpkg".
	if expr, addr, ok := cut(expr, ":"); ok {
		item := s.Lookup(expr)
		if item == nil {
			return nil
		}
		if item.Kind == ItemFile {
			_, f := s.FileByName(item.Name)
			base, eof := s.FileRange(f.Package)
			text := s.Text(base, eof)
			lo, hi, err := addrToByteRange(addr, 0, text)
			if err != nil {
				s.ErrorAt(token.NoPos, "cannot evaluate address %s: %v", addr, err)
				panic("bad addr")
			}
			return &Item{
				Kind:  ItemPos,
				Outer: item,
				Pos:   base + token.Pos(lo),
				End:   base + token.Pos(hi),
			}
		}
		panic("bad addr")
	}
	if strings.Contains(expr, "/") {
		return &Item{Kind: ItemDir, Name: expr}
	}
	if strings.HasSuffix(expr, ".go") {
		return &Item{Kind: ItemFile, Name: expr}
	}

	name, rest, more := cut(expr, ".")
	item := lookupInScope(s.pkgs[0].Types.Scope(), name)
	if item == nil {
		return nil
	}
	item.Name = name
	for more {
		name, rest, more = cut(rest, ".")
		item = lookupIn(s.pkgs[0], item, name)
		if item == nil {
			return nil
		}
		item.Name = item.Outer.Name + "." + name
	}
	return item
}

func (s *Snapshot) FileRange(pos token.Pos) (start, end token.Pos) {
	tf := s.Fset().File(pos)
	start = token.Pos(tf.Base())
	return start, start + token.Pos(tf.Size())
}

func lookupInScope(scope *types.Scope, expr string) *Item {
	obj := scope.Lookup(expr)
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

func lookupIn(p *packages.Package, outer *Item, name string) *Item {
	switch outer.Kind {
	case ItemConst:
		return nil
	case ItemType:
		// Look for method, field.
		return lookupType(p, outer, outer.Obj.Type(), name)
	case ItemVar:
		// If unnamed struct or interface, look in type.
		typ := outer.Obj.Type().Underlying()
		if ptr, ok := typ.(*types.Pointer); ok {
			typ = ptr.Elem().Underlying()
		}
		switch typ := typ.(type) {
		default:
			fmt.Printf("LOOKUP IN %T\n", typ)
		case *types.Struct, *types.Interface:
			return lookupType(p, outer, typ, name)
		}
	case ItemFunc:
		// Look for declaration inside function.
		item := lookupInScope(outer.Obj.(*types.Func).Scope(), name)
		if item != nil {
			item.Outer = outer
			return item
		}
	case ItemField:
		return lookupType(p, outer, outer.Obj.Type(), name)
	}
	return nil
}

func lookupType(p *packages.Package, outer *Item, typ types.Type, name string) *Item {
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

func (s *Snapshot) FileAt(pos token.Pos) (*packages.Package, *ast.File) {
	for _, p := range s.pkgs {
		for _, file := range p.Syntax {
			tfile := s.fset.File(file.Pos())
			if tfile.Base() <= int(pos) && int(pos) <= tfile.Base()+tfile.Size() {
				return p, file
			}
		}
	}
	return nil, nil
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

type newImport struct {
	id  string
	pkg *types.Package
}

func (s *Snapshot) NeedImport(pos token.Pos, id string, pkg *types.Package) string {
	_, file := s.FileAt(pos)
	if file == nil {
		fmt.Println(s.Position(pos))
		panic("no file")
	}

	for _, imp := range file.Imports {
		if importPath(imp) == pkg.Path() {
			name := importName(imp)
			if name == "" {
				name = pkg.Name()
			}
			if name == id || id == "" {
				// TODO check scope
				return name
			}
		}
	}

	if id == "" {
		id = pkg.Name()
	}
	filename := s.Position(file.Package).Filename
	added := s.added[filename]
	key := newImport{id, pkg}
	for _, p := range added {
		if p == key {
			return id
		}
	}
	s.added[filename] = append(s.added[filename], key)
	return id
}

func (s *Snapshot) addImports() {
	for file, list := range s.added {
		s.addImportList(file, list)
	}
}

func (s *Snapshot) addImportList(file string, list []newImport) {
	_, f := s.FileByName(file)
	imps := f.Decls
	for i, d := range f.Decls {
		if d, ok := d.(*ast.GenDecl); !ok || d.Tok != token.IMPORT {
			imps = f.Decls[:i]
			break
		}
	}

	// Assign each import to an import statement.
	needs := make(map[*ast.ImportSpec][]newImport)
	impOf := make(map[*ast.ImportSpec]*ast.GenDecl)
	var firstImp *ast.GenDecl
	for _, need := range list {
		// Find an import decl to add to.
		// Same logic as go fix.
		// Find an import decl to add to.
		var (
			bestMatch = -1
			bestSpec  *ast.ImportSpec
		)
		for i := range imps {
			imp := imps[i].(*ast.GenDecl)
			// Do not add to import "C", to avoid disrupting the
			// association with its doc comment, breaking cgo.
			if declImports(imp, "C") {
				continue
			}
			if firstImp == nil {
				firstImp = imp
			}

			// Compute longest shared prefix with imports in this block.
			for j := range imp.Specs {
				spec := imp.Specs[j].(*ast.ImportSpec)
				impOf[spec] = imp
				n := matchLen(importPath(spec), need.pkg.Path())
				if n > bestMatch {
					bestMatch = n
					bestSpec = spec
				}
			}
		}
		needs[bestSpec] = append(needs[bestSpec], need)
	}

	makeBlock := func(imp *ast.GenDecl) {
		if imp.Lparen == token.NoPos {
			imp.Lparen = imp.TokPos + token.Pos(len("import"))
			s.InsertAt(imp.Lparen, "(")
			imp.Rparen = imp.End()
			s.InsertAt(imp.Rparen+1, ")") // TODO: skip over comment; the +1 is to avoid conflict with the InsertAt in needs[nil] below
		}
	}
	// Add imports near each target spec.
	for i := range imps {
		imp := imps[i].(*ast.GenDecl)
		for j := range imp.Specs {
			spec := imp.Specs[j].(*ast.ImportSpec)
			if needs[spec] == nil {
				continue
			}
			makeBlock(impOf[spec])
			for _, need := range needs[spec] {
				id := need.id
				if id == need.pkg.Name() {
					id = ""
				}
				s.InsertAt(spec.Pos(), fmt.Sprintf("%s %q\n", id, need.pkg))
			}
		}
	}

	if needs[nil] != nil {
		// Imports we didn't know what to do with.
		// Add new block to first (non-C) import, if any.
		var buf bytes.Buffer
		kind := -1
		all := needs[nil]
		sort.Slice(all, func(i, j int) bool {
			if ki, kj := pathKind(all[i].pkg.Path()), pathKind(all[j].pkg.Path()); ki != kj {
				return ki < kj
			}
			return all[i].pkg.Path() < all[j].pkg.Path()
		})
		for _, need := range all {
			if k := pathKind(need.pkg.Path()); k != kind {
				buf.WriteString("\n")
				kind = k
			}
			id := need.id
			if id == need.pkg.Name() {
				id = ""
			}
			fmt.Fprintf(&buf, "%s %q\n", id, need.pkg.Path())
		}
		imp := firstImp
		if imp != nil {
			makeBlock(imp)
			s.InsertAt(imp.Rparen, buf.String())
		} else {
			pos := f.Name.End()
			if len(imps) > 0 {
				pos = imps[len(imps)-1].End()
			}
			if len(needs[nil]) == 1 {
				s.InsertAt(pos, "\nimport "+buf.String()[1:])
			} else {
				s.InsertAt(pos, "\nimport ("+buf.String()+")")
			}
		}
	}
}

// declImports reports whether gen contains an import of path.
func declImports(gen *ast.GenDecl, path string) bool {
	if gen.Tok != token.IMPORT {
		return false
	}
	for _, spec := range gen.Specs {
		impspec := spec.(*ast.ImportSpec)
		if importPath(impspec) == path {
			return true
		}
	}
	return false
}

// matchLen returns the length of the longest prefix shared by x and y.
func matchLen(x, y string) int {
	if pathKind(x) != pathKind(y) {
		return -1
	}

	i := 0
	for i < len(x) && i < len(y) && x[i] == y[i] {
		i++
	}
	return i
}

func pathKind(x string) int {
	first, _, _ := cut(x, "/")
	if strings.Contains(first, ".") {
		return 2
	}
	if first == "cmd" {
		return 1
	}
	return 0
}

func (s *Snapshot) FileByName(name string) (*packages.Package, *ast.File) {
	if !filepath.IsAbs(name) {
		name = filepath.Join(s.r.dir, name)
	}
	for _, p := range s.pkgs {
		for _, file := range p.Syntax {
			if s.Position(file.Package).Filename == name {
				return p, file
			}
		}
	}
	return nil, nil
}

func (s *Snapshot) PackageAt(pos token.Pos) *packages.Package {
	pkg, _ := s.FileAt(pos)
	return pkg
}

// A buffer is a queue of edits to apply to a file text.
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
