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
	"path"
	"path/filepath"
	"strconv"
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
	dir, err := filepath.EvalSymlinks(dir)
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

	for _, pkg := range list {
		// pkg.Module should always be non-nil,
		// but when working in $GOROOT it is not filled in for cmd
		// TODO: Need to figure out whether it's not filled in
		// for std/cmd packages even when outside std/cmd.
		if pkg.Module != nil && !pkg.Module.Main {
			return nil, fmt.Errorf("cannot refactor package %s outside current module", pkg.PkgPath)
		}
	}

	r := &Refactor{
		Stdout:  os.Stdout,
		Stderr:  os.Stderr,
		dir:     dir,
		modRoot: modRoot,
		modPath: modPath,
		targets: list,
	}
	return r, nil
}

func (r *Refactor) Importers(snap *Snapshot) ([]string, error) {
	var overlay map[string][]byte
	if snap != nil {
		overlay = snap.files.overlay()
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
	edits   map[string]*edit.Buffer
	added   map[string][]string
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
		r:     r,
		old:   base,
		fset:  token.NewFileSet(),
		edits: make(map[string]*edit.Buffer),
		added: make(map[string][]string),
	}
	if base != nil {
		for name, edit := range base.edits {
			s.files.cacheRead(name, edit.Bytes())
		}
	}
	cfg := &packages.Config{
		Mode:      packages.NeedName | packages.NeedSyntax | mode,
		Dir:       r.dir,
		Tests:     true,
		Fset:      s.fset,
		ParseFile: s.files.ParseFile,
		Overlay:   s.files.overlay(),
	}
	pkgs, err := packages.Load(cfg, append([]string{"."}, extra...)...)
	if err != nil {
		return nil, err
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

func (s *Snapshot) Edit(lo, hi token.Pos, repl string) {
	plo := s.Position(lo)
	phi := s.Position(hi)
	file := plo.Filename
	if s.edits[file] == nil {
		s.edits[file] = edit.NewBuffer(s.files.cacheRead(file, nil))
	}
	s.edits[file].Replace(plo.Offset, phi.Offset, repl)
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

func (s *Snapshot) File(pos token.Pos) string {
	return s.fset.Position(pos).Filename
}

func (s *Snapshot) oldBytes(name string) []byte {
	if s.old != nil {
		b := s.old.oldBytes(name)
		if b != nil {
			return b
		}
	}
	return s.files.cacheRead(name, nil)
}

func (s *Snapshot) Diff() ([]byte, error) {
	var diffs []byte
	for _, p := range s.pkgs {
		for _, file := range p.Syntax {
			name := s.File(file.Package)
			var newBytes []byte
			if buf, ok := s.edits[name]; ok {
				newBytes = buf.Bytes()
			} else {
				if text := s.files.cacheRead(name, nil); text != nil {
					newBytes = text
				}
			}
			if newBytes == nil {
				continue
			}

			rel, err := filepath.Rel(s.r.modRoot, name)
			d, err := diff.Diff("old/"+rel, s.oldBytes(name), "new/"+rel, newBytes)
			if err != nil {
				return nil, err
			}
			diffs = append(diffs, d...)
		}
	}
	return diffs, nil
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
			deleteUnusedImports(fset, file)

			var out bytes.Buffer
			if err := format.Node(&out, fset, file); err != nil {
				continue
			}
			s.edits[name] = edit.NewBuffer(out.Bytes())
		}
	}
}

func (s *Snapshot) Write() error {
	failed := false
	for _, p := range s.pkgs {
		for _, file := range p.Syntax {
			name := s.File(file.Package)
			buf, ok := s.edits[name]
			if !ok {
				continue
			}
			if err := ioutil.WriteFile(name, buf.Bytes(), 0666); err != nil {
				fmt.Fprintf(s.r.Stderr, "%s\n", err)
				failed = true
			}
		}
	}
	if failed {
		return fmt.Errorf("errors writing files")
	}
	return nil
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
	Obj   types.Object
	Outer *Item
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

func (s *Snapshot) Lookup(expr string) *Item {
	// Special cases for directory, file, as in "mv Thing ../newpkg".
	if strings.Contains(expr, "/") {
		return &Item{Kind: ItemDir, Name: expr}
	}
	if strings.HasSuffix(expr, ".go") {
		return &Item{Kind: ItemFile, Name: expr}
	}

	name, rest, more := cut(expr, ".")
	item := lookupTop(s.pkgs[0], name)
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

func lookupTop(p *packages.Package, expr string) *Item {
	obj := p.Types.Scope().Lookup(expr)
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

func InspectAST(n ast.Node, f func(stack []ast.Node)) {
	var stack []ast.Node
	var stackPos int

	ast.Inspect(n, func(n ast.Node) bool {
		if n == nil {
			stackPos++
			return true
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

func (s *Snapshot) findFile(pos token.Pos) (*packages.Package, *ast.File) {
	for _, p := range s.pkgs {
		for _, file := range p.Syntax {
			if file.Pos() <= pos && pos <= file.End() {
				return p, file
			}
		}
	}
	return nil, nil
}

func (s *Snapshot) SyntaxAt(pos token.Pos) []ast.Node {
	_, file := s.findFile(pos)
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

func (s *Snapshot) NeedImport(pos token.Pos, id, pkg string) {
	_, file := s.findFile(pos)
	if file == nil {
		fmt.Println(s.Position(pos))
		panic("no file")
	}

	for _, imp := range file.Imports {
		if importPath(imp) == pkg {
			name := importName(imp)
			if name == "" {
				name = path.Base(pkg)
			}
			if id == name {
				return
			}
		}
	}

	filename := s.Position(file.Package).Filename
	added := s.added[filename]
	key := id + " " + pkg
	for _, p := range added {
		if p == key {
			return
		}
	}
	s.added[filename] = append(s.added[filename], key)

	ident := ""
	if id != path.Base(pkg) {
		ident = id + " "
	}

	s.Edit(file.Name.End(), file.Name.End(), "\nimport "+ident+strconv.Quote(pkg))
}

func (s *Snapshot) TargetFileByName(name string) (*packages.Package, *ast.File) {
	if !filepath.IsAbs(name) {
		name = filepath.Join(s.r.dir, name)
	}
	for _, p := range s.targets {
		for _, file := range p.Syntax {
			if s.Position(file.Package).Filename == name {
				return p, file
			}
		}
	}
	return nil, nil
}

func (s *Snapshot) PackageAt(pos token.Pos) *packages.Package {
	for _, p := range s.targets {
		for _, file := range p.Syntax {
			if file.Pos() <= pos && pos <= file.End() {
				return p
			}
		}
	}
	return nil
}
