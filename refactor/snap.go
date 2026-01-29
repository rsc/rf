// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package refactor

import (
	"bytes"
	"crypto/sha256"
	"encoding/json"
	"fmt"
	"go/ast"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"sort"
	"strings"
	"time"

	"rsc.io/rf/diff"
)

const (
	debugLoad      = false
	debugApply     = false
	debugBuildIDs  = false
	debugTypeCheck = false
)

// A Snapshot is a base set of Packages and their parsed source files, plus a
// set of concurrent edits to be made to those files.
type Snapshot struct {
	parent   *Snapshot
	fset     *token.FileSet
	target   *Package
	packages []*Package
	pkgGraph *pkgGraph
	importer types.Importer

	// edits contains edits made to files by this Snapshot. It's keyed by short
	// path and only contains entries for files that have been modified.
	edits map[string]*Edit

	// files contains the contents of files before any edits in this Snapshot.
	// It's keyed by short path (File.Name)
	files map[string]*File

	Errors *ErrorList

	r     *Refactor
	cache *buildCache
	sizes types.Sizes
}

// From local import path to package path (used for vendored packages in std)
type importMap map[string]string

// Lookup returns the pkgPath for the given import path.
func (im importMap) Lookup(importPath string) string {
	if pkgPath, ok := im[importPath]; ok {
		return pkgPath
	}
	return importPath
}

type Package struct {
	Name            string
	Dir             string
	ID              string
	PkgPath         string
	ForTest         string
	Files           []*File // Sorted by File.Name
	Imports         []string
	InCurrentModule bool
	Export          string
	BuildID         string
	ImportMap       importMap

	Types     *types.Package
	TypesInfo *types.Info
	Sizes     types.Sizes
}

func (p *Package) String() string { return p.PkgPath }

// File represents a source file, including both its text and parsed forms.
// Files are tracked by the buildCache and are deeply immutable.
type File struct {
	Name     string // Short path (either relative to r.dir or absolute)
	Text     []byte
	Syntax   *ast.File // Parsed form of Text
	Imports  []string  // Local import paths, derived from Syntax
	Modified bool      // Modified from on-disk file (or does not exist on disk)
	Deleted  bool      // Tombstone marking a deleted File. Only Name is valid.
	Hash     string    // SHA256(Name+Text)
}

type cachedTypeInfo struct {
	pkg  *types.Package
	info *types.Info
}

type buildCache struct {
	dir   string
	r     *Refactor
	fset  *token.FileSet
	types map[string]*cachedTypeInfo // keyed by Package.BuildID
	files map[string]*File           // keyed by hash(name+content)
}

func (s *Snapshot) Refactor() *Refactor { return s.r }

func (s *Snapshot) ErrorAt(pos token.Pos, format string, args ...any) {
	msg := fmt.Sprintf(format, args...)
	msg = strings.TrimRight(msg, "\n")
	msg = strings.ReplaceAll(msg, "\n", "\n\t")
	if pos == token.NoPos {
		s.Errors.Add(&Error{Msg: msg})
	} else {
		s.Errors.Add(&Error{Pos: s.Position(pos), Msg: msg})
	}
}

func (s *Snapshot) Fset() *token.FileSet { return s.fset }

func (s *Snapshot) Target() *Package {
	return s.target
}

func (s *Snapshot) Packages() []*Package {
	return s.packages
}

// Snapshots returns the latest Snapshot set. The caller should perform the same
// refactoring on each Snapshot and then call r.Commit(). On the first call, it
// loads all packages.
func (r *Refactor) Snapshots() ([]*Snapshot, error) {
	if r.snapshots == nil {
		snapshots, err := r.load()
		if err != nil {
			return nil, err
		}
		r.snapshots = snapshots
	}
	return r.snapshots, nil
}

// load reads all packages in the current module into r and creates the initial
// Snapshots.
func (r *Refactor) load() ([]*Snapshot, error) {
	var snapshots []*Snapshot
	for _, config := range r.Configs.c {
		s, err := r.load1(config)
		if err != nil {
			return nil, err
		}
		snapshots = append(snapshots, s...)
	}
	return snapshots, nil
}

func (r *Refactor) load1(config Config) ([]*Snapshot, error) {
	dir := r.dir
	dir, err := filepath.Abs(dir)
	if err != nil {
		return nil, err
	}
	dir = filepath.Clean(dir)

	cmd := exec.Command("go", "env", "GOMOD")
	cmd.Dir = dir
	cmd.Env = append(os.Environ(), "PWD="+dir)
	bmod, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("loading module: %v\n%s", err, bmod)
	}
	mod := strings.TrimSpace(string(bmod))
	if filepath.Base(mod) != "go.mod" {
		return nil, fmt.Errorf("no module found for: %s", dir)
	}
	r.modRoot = filepath.Dir(mod)

	cmd = exec.Command("go", "mod", "edit", "-json")
	cmd.Dir = dir
	cmd.Env = append(os.Environ(), "PWD="+dir)
	js, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("loading module: %v\n%s", err, bmod)
	}
	var m struct {
		Module struct {
			Path string
		}
	}
	if err := json.Unmarshal(js, &m); err != nil {
		return nil, fmt.Errorf("loading module: %v\n%s", err, bmod)
	}
	r.modPath = m.Module.Path
	isStd := r.modPath == "std" || r.modPath == "cmd"

	cfgFlags, cfgEnvs, err := config.flagsEnvs()
	if err != nil {
		return nil, err
	}
	cmdList := append(append([]string{"list", "-e", "-pgo=off", "-json", "-compiled", "-test", "-deps"}, cfgFlags...), "./...")
	cmd = exec.Command("go", cmdList...)
	cmd.Dir = r.modRoot
	cmd.Env = append(append(os.Environ(), "PWD="+r.modRoot), cfgEnvs...)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	err = cmd.Run()
	if err != nil && !(bytes.HasPrefix(stdout.Bytes(), []byte("{")) || !bytes.HasPrefix(stdout.Bytes(), []byte("}"))) {
		return nil, fmt.Errorf("listing module: %v\n%s%s", err, stderr.Bytes(), stdout.Bytes())
	}
	dec := json.NewDecoder(bytes.NewReader(stdout.Bytes()))

	errs := new(ErrorList)
	defer errs.flushOnPanic(r.Stderr)

	// Parse the packages and construct several package graphs.
	//
	// Each package can appear several times in "go list", compiled on behalf of
	// different test binaries. For example, in addition to the non-test
	// "runtime" package, we also see "runtime [runtime.test]", "runtime
	// [internal/abi.test]", and "runtime [internal/cpu.test]". "runtime
	// [runtime.test]" is the runtime package including _test.go files that are
	// in package runtime, ultimately on behalf of compiling the runtime_test
	// package. This split of the runtime package means that any packages on the
	// path between runtime_test and runtime must also have a "[runtime.test]"
	// variant.
	//
	// Partly to reduce the number of packages we need to consider, we assume
	// any "p [p.test]" package is a superset of "p", and thus we can substitute
	// "p [p.test]" for "p". (There are at least two cases where this isn't
	// true: "p [p.test]" can add a method p.T.X that overrides a field p.T.X
	// and thus changes the resolution of p.T.X in importers of p, and "p
	// [p.test]" can shadow names in the universe scope that affect types in
	// "p". Both are demonstrated here: https://go.dev/play/p/ynQmIAHZVZu We
	// assume such cases are exceedingly rare.)
	//
	// "runtime [internal/abi.test]", on the other hand, is identical to
	// "runtime" (assuming that "p [p.test]" really is a superset of "p"), but
	// because package runtime is on the path between internal/abi and
	// internal/abi_test, it has a separate package to express the dependency
	// chain "internal/abi.test" -> "runtime [internal/abi.test]" ->
	// "internal/abi [internal/abi.test]".
	//
	// Where this gets tricky is that each Snapshot can only have a single
	// variant of any given package. (We rely on this in many places. File paths
	// are unique. There's only a single "target" package per Snapshot.
	// Identifier lookups return a unique types.Object.) It's tempting to just
	// take all of the "p [p.test]" variants as canonical for each "p" because
	// they're supersets of "p", but if we do this across the whole package
	// graph it can introduce import cycles.
	//
	// For example, suppose there are two packages, a and b, that have no import
	// relationship. But a/a_test.go imports b and b/b_test.go imports a. When
	// we're only considering the graph for a.test or the graph for b.test, this
	// is fine. But if we attempt to merge these into a single graph, using "a
	// [a.test]" for package a and "b [b.test]" for package b, this creates an
	// import cycle.
	//
	// Hence, we may need multiple Snapshots to represent non-cyclic import
	// graphs. We could have one Snapshot per test target, but this is wasteful
	// because these import cycles are actually quite rare. Instead, we do a
	// best-effort merging of package graphs to reduce the number of Snapshots.
	//
	// We start by building all of these package graphs separately: a base graph
	// for all non-test variants, and one per test target. We then build up a
	// "primary" graph, which starts with the base graph and attempts to merge
	// each test target graph into the primary graph. If we can merge a test
	// target graph without introducing cycles, we keep that update to the
	// primary graph. If not, we undo that update and create a secondary graph
	// just for that test target.
	//
	// This "single variant" assumption this approach satisfies is all over the
	// code, so this is a pragmatic choice. But it has some unfortunate
	// limitations:
	//
	// - It's inefficient. Each Snapshot needs a complete copy of all Packages.
	// Each operation has to be done on all Snapshots, even if the package its
	// operating on is identical in all Snapshots. Generally, it requires
	// repeating a lot of work between each Snapshot. (It would be even better
	// if we could also make changes to low-level packages that don't touch
	// exports not require re-type-checking everything above it, but this would
	// break types.Object equality across packages.)
	//
	// - If the Snapshots diverge, it's very hard to usefully explain what went
	// wrong.
	//
	// - Sometimes commands only make sense on certain Snapshots. For example,
	// if a command refers to an identifier that's in a test file. It we pushed
	// package variants down into Item resolution, it could understand when some
	// variants have that item and others don't and simply return the
	// resolutions that work. It would only be an error if the item couldn't be
	// resolved in any variant. Instead, we have a complex system of
	// "precondition errors" that pushes this problem up to the user of this
	// package (and often gets things wrong, like if a command is doing a lot of
	// edits and only some preconditions are met).
	//
	// - Ideally we would include any p_test package in the current directory as
	// a target, but this is both too inefficient (would require 2x as many
	// Snapshots) and would really push on the "precondition" issue.

	pkgByID := make(map[string]*Package)
	needExportData := make(map[string]bool) // Keyed by package ID
	base := newPkgGraph("base")
	graphByTest := make(map[string]*pkgGraph) // keyed by ForTest
	for {
		var jp jsonPackage
		err := dec.Decode(&jp)
		if err == io.EOF {
			break
		}
		if err != nil {
			return nil, fmt.Errorf("loading packages: %v", err)
		}
		if jp.Name == "" {
			continue
		}

		// The forms we expect to see are:
		//	p (the regular package)
		//	q [p.test] (package q compiled with test code for the p test binary)
		//	p_test [p.test] (p_test compiled for the test binary)
		id := jp.ImportPath
		pkgPath := strings.TrimSuffix(id, " ["+jp.ForTest+".test]")
		if strings.HasSuffix(pkgPath, ".test") {
			// Ignore test binaries.
			continue
		}

		p := new(Package)
		p.Name = jp.Name
		p.Dir = jp.Dir
		p.ID = id
		p.PkgPath = pkgPath
		p.ForTest = jp.ForTest

		if isStd {
			// Packages in std and cmd don't have a Module field, but can't
			// reach outside the std or cmd modules anyway.
			p.InCurrentModule = true
		} else {
			// The module version is "" for anything in the workspace.
			p.InCurrentModule = jp.Module != nil && jp.Module.Version == ""
		}

		if pkgByID[p.ID] != nil {
			return nil, fmt.Errorf("duplicate package ID %q", p.ID)
		}
		pkgByID[p.ID] = p

		// jp.Imports is a list of package IDs. Since we're splitting package
		// variants into different package graphs anyway, it's much easier to
		// just flatten down to package path, which is unique within each
		// package graph.
		var imports []string
		have := make(map[string]bool)
		for _, impID := range jp.Imports {
			if impID == "C" {
				// cgo has been compiled out already
				continue
			}
			p1 := pkgByID[impID]
			if p1 == nil {
				return nil, fmt.Errorf("package %q imports %q, which is missing from go list", p.ID, impID)
			}
			if have[p1.PkgPath] {
				// Imports can be listed multiple times.
				continue
			}
			have[p1.PkgPath] = true
			imports = append(imports, p1.PkgPath)
		}
		sort.Strings(imports)
		p.Imports = imports

		// Convert the IDs in the ImportMap to package paths as well.
		//
		// TODO: Introduce a type for local import paths so import paths and
		// package paths aren't so easy to mix up?
		var importMap map[string]string
		for localPath, id := range jp.ImportMap {
			p1 := pkgByID[id]
			if p1 == nil {
				return nil, fmt.Errorf("package %q imports %q as %q, which is missing from go list", p.ID, id, localPath)
			}
			if localPath == p1.PkgPath {
				// Typical for test-related package IDs.
				continue
			}
			if importMap == nil {
				importMap = make(map[string]string)
			}
			importMap[localPath] = p1.PkgPath
		}
		p.ImportMap = importMap

		// Add this package to the appropriate package graph.
		g := base
		if p.ForTest != "" {
			g = graphByTest[p.ForTest]
			if g == nil {
				g = newPkgGraph(p.ForTest)
				graphByTest[p.ForTest] = g
			}
		}
		g.add(p)
		if debugLoad {
			fmt.Println("new package", id, "added to graph", g.name)
		}

		if !p.InCurrentModule {
			// Outside current module, so not updating.
			// Load from export data.
			// Don't bother with source files.
			needExportData[p.ID] = true
			continue
		}

		if len(jp.CompiledGoFiles) == 0 {
			// We couldn't get a list of files. Check for loading errors.
			if jp.Error != nil {
				// Report a package loading error now.
				errs.Add(fmt.Errorf("%s", strings.TrimSuffix(jp.Error.Err, "\n")))
				continue
			}
			if jp.DepsErrors != nil {
				// Report errors loading dependencies now given we could not
				// get a list of files.
				for _, depError := range jp.DepsErrors {
					errs.Add(fmt.Errorf("error loading dependency: %s", strings.TrimSuffix(depError.Err, "\n")))
				}
				continue
			}
		}

		// Set up for loading from source code.
		for _, name := range jp.CompiledGoFiles {
			if strings.HasSuffix(name, ".s") { // surprise!
				continue
			}
			if !filepath.IsAbs(name) {
				name = filepath.Join(p.Dir, name)
			}
			name = r.shortPath(name)
			f, err := r.cache.newFile(name)
			if err != nil {
				errs.Add(err)
				continue
			}
			p.Files = append(p.Files, f)
		}
	}
	if err := errs.Err(); err != nil {
		// Parsing failed.
		return nil, err
	}

	if debugLoad {
		fmt.Println(len(graphByTest), "initial test graphs")
	}

	// Load export data. We do this for only the packages we need because it
	// requires compiling the packages, which can be quite slow.
	if len(needExportData) > 0 {
		needExportPaths := make(map[string]bool)
		for pkgID := range needExportData {
			pkg := pkgByID[pkgID]
			if pkg.ForTest == "" {
				needExportPaths[pkg.PkgPath] = true
			} else {
				// We can't ask for test packages directly, so depend on -test
				// -deps to provide the package we're looking for.
				needExportPaths[pkg.ForTest] = true
			}
		}
		paths := make([]string, 0, len(needExportPaths))
		for k := range needExportPaths {
			paths = append(paths, k)
		}
		sort.Strings(paths)
		cmdList := append(append([]string{"list", "-e", "-json", "-export", "-test", "-deps"}, cfgFlags...), paths...)
		cmd = exec.Command("go", cmdList...)
		cmd.Dir = r.modRoot
		cmd.Env = append(append(os.Environ(), "PWD="+r.modRoot), cfgEnvs...)
		var stdout, stderr bytes.Buffer
		cmd.Stdout = &stdout
		cmd.Stderr = &stderr
		err = cmd.Run()
		if err != nil {
			return nil, fmt.Errorf("listing exports: %v\n%s%s", err, stderr.Bytes(), stdout.Bytes())
		}
		dec := json.NewDecoder(bytes.NewReader(stdout.Bytes()))

		for {
			var jp jsonPackage
			err := dec.Decode(&jp)
			if err == io.EOF {
				break
			}
			if err != nil {
				return nil, fmt.Errorf("loading exports: %v", err)
			}

			id := jp.ImportPath
			if needExportData[id] {
				pkgByID[id].Export = jp.Export
			}
		}
	}

	// Merge graphs as long as we don't create cycles. We use a simple greedy
	// approach, and apply a heuristic to merge the graphs with fewer edges
	// first since they're less likely to create cycles.
	var testGraphs []*pkgGraph
	for _, g := range graphByTest {
		testGraphs = append(testGraphs, g)
	}
	sort.Slice(testGraphs, func(i, j int) bool {
		if testGraphs[i].nEdges != testGraphs[j].nEdges {
			return testGraphs[i].nEdges < testGraphs[j].nEdges
		}
		return testGraphs[i].name < testGraphs[j].name
	})
	if c := base.findCycle(true); c != nil {
		return nil, fmt.Errorf("base package graph contains cycle: %s", c)
	}
	allGraphs := []*pkgGraph{base}
	for _, testGraph := range testGraphs {
		// Try merging this test package graph into the primary graph. This
		// works for the vast majority of test packages in std.
		primary := &allGraphs[0]
		g := (*primary).merge(testGraph)
		c := g.findCycle(debugLoad)
		if c == nil {
			// We were able to add this test package graph without introducing
			// cycles. Keep it.
			g.name = "primary"
			if debugLoad {
				fmt.Println("merged", testGraph.name, "into", g.name)
			}
			*primary = g
			continue
		}
		// This test package adds cycles to the primary graph. As another
		// heuristic, try adding it in to the previous secondary graph. On std,
		// this is incredibly effective.
		if len(allGraphs) > 1 {
			pprev := &allGraphs[len(allGraphs)-1]
			g := (*pprev).merge(testGraph)
			if g.findCycle(false) == nil {
				// Great. Commit it to that graph.
				g.name = (*pprev).name
				if debugLoad {
					fmt.Println("merged", testGraph.name, "into", g.name)
				}
				*pprev = g
				continue
			}
		}
		// Give up an create a new secondary graph.
		if debugLoad {
			fmt.Println("new secondary graph", testGraph.name, "because", c)
		}
		// We need to clone all of the Packages in the base graph because
		// Packages cache things like the build ID and type-checking results,
		// which are sensitive to the graph the Package is in, and have some
		// mutable fields like Files.
		g = base.clonePackages().merge(testGraph)
		g.name = testGraph.name
		if c := g.findCycle(true); c != nil {
			// This shouldn't happen.
			return nil, fmt.Errorf("combining test package graph %s into base graph created a cycle: %s", testGraph.name, c)
		}
		allGraphs = append(allGraphs, g)
	}

	// Compute build IDs.
	for _, g := range allGraphs {
		errs.Add(r.setBuildIDs(g))
	}
	if err := errs.Err(); err != nil {
		return nil, err
	}

	if debugLoad {
		fmt.Println(len(allGraphs), "total merged graphs")
		if true {
			for i, g := range allGraphs {
				fmt.Printf("-- graph %d --\n", i)
				g.dump(os.Stdout)
			}
		}
	}

	// Create Snapshots from graphs.
	var snapshots []*Snapshot
	for _, graph := range allGraphs {
		s := &Snapshot{
			r:        r,
			pkgGraph: graph,
			edits:    make(map[string]*Edit),
			files:    make(map[string]*File),
			Errors:   new(ErrorList),
			fset:     r.cache.fset,
			packages: graph.packages(),
		}
		snapshots = append(snapshots, s)

		for _, p := range s.packages {
			for _, f := range p.Files {
				s.files[f.Name] = f
			}
			// Remember the target package,
			// which is the one in the current directory.
			//
			// TODO: Ideally we would include the p_test package, too (if there
			// is one), but that requires creating more Snapshots. (If we just
			// allowed package variants, item resolution could understand p_test
			// and this would scale much better.)
			if p.Dir == dir && !strings.HasSuffix(p.PkgPath, "_test") {
				if s.target != nil {
					panic(fmt.Sprintf("found two target packages for directory %q: %q and %q", dir, s.target, p))
				}
				s.target = p
			}
		}

		if s.Errors.Err() == nil {
			s.typeCheck()
		}
		if err := s.Errors.Err(); err != nil {
			errs.Add(err)
		}
	}
	if err := errs.Err(); err != nil {
		return nil, err
	}

	return snapshots, nil
}

// Apply applies edits to all Snapshots, type-checks the resulting files, and
// creates a new set of current Snapshots. If there are any type errors in the
// new Snapshots, it returns an ErrorList.
func (r *Refactor) Apply() error {
	newSnapshots := make([]*Snapshot, len(r.snapshots))
	var errs ErrorList
	for i, snap := range r.snapshots {
		newSnapshot, err := snap.apply()
		switch err := err.(type) {
		case nil:
		case *ErrorList:
			errs.Add(err)
		default:
			return err
		}
		newSnapshots[i] = newSnapshot
	}
	if err := errs.Err(); err != nil {
		return err
	}
	r.snapshots = newSnapshots
	return nil
}

func (oldS *Snapshot) apply() (*Snapshot, error) {
	s := &Snapshot{
		r:        oldS.r,
		parent:   oldS,
		fset:     oldS.fset,
		edits:    make(map[string]*Edit),
		pkgGraph: newPkgGraph(oldS.pkgGraph.name),
		files:    make(map[string]*File),
		Errors:   oldS.Errors,
		// exp:       s.exp, //  should use cache from now on
		sizes: oldS.sizes,
	}
	defer s.Errors.flushOnPanic(s.r.Stderr)

	if debugApply {
		fmt.Printf("start apply, creating Snapshot %p from %p\n", s, oldS)
	}

	for _, oldP := range oldS.packages {
		if !oldP.InCurrentModule { // immutable w/ immutable dependencies
			if debugApply {
				fmt.Printf("apply: copying %s %p to new Snapshot\n", oldP.ID, oldP)
			}
			s.pkgGraph.add(oldP)
			for _, f := range oldP.Files {
				s.files[f.Name] = f
			}
			continue
		}
		p := &Package{
			Name:            oldP.Name,
			Dir:             oldP.Dir,
			ID:              oldP.ID,
			PkgPath:         oldP.PkgPath,
			ForTest:         oldP.ForTest,
			InCurrentModule: oldP.InCurrentModule,
			ImportMap:       oldP.ImportMap,
		}
		if oldS.target == oldP {
			s.target = p
		}
		if debugApply {
			fmt.Printf("apply %s %p => %s %p in Snapshot %p\n", oldP.ID, oldP, p.ID, p, s)
		}

		// Build file list.
		for _, oldF := range oldP.Files {
			ed := oldS.edits[oldF.Name]
			if ed == nil {
				p.Files = append(p.Files, oldF)
				continue
			}
			if ed.Delete {
				f := &File{
					Name:    oldF.Name,
					Deleted: true,
				}
				p.Files = append(p.Files, f)
				continue
			}
			// Use modified file.
			text, err := ed.NewText()
			if err != nil {
				s.ErrorAt(token.NoPos, "%s: %v", oldF.Name, err)
				continue
			}
			f, err := s.r.cache.newFileText(oldF.Name, text, true)
			if err != nil {
				s.Errors.Add(err)
				continue
			}
			p.Files = append(p.Files, f)
		}
		for _, f := range p.Files {
			s.files[f.Name] = f
		}

		// Gather imports from files.
		p.Imports = s.pkgImportsFromFiles(p)
		if debugApply && !reflect.DeepEqual(oldP.Imports, p.Imports) {
			fmt.Printf("apply: updated imports\n  old: %s\n  new: %s\n", oldP.Imports, p.Imports)
		}

		// Add to new package graph.
		s.pkgGraph.add(p)
	}
	s.packages = s.pkgGraph.packages()

	if s.Errors.Err() == nil {
		s.Errors.Add(s.r.setBuildIDs(s.pkgGraph))
	}
	if s.Errors.Err() == nil {
		s.typeCheck()
	}
	if err := s.Errors.Err(); err != nil {
		return nil, err
	}
	return s, nil
}

func (s *Snapshot) pkgImportsFromFiles(p *Package) []string {
	var list []string
	have := make(map[string]bool)
	for _, f := range p.Files {
		for _, imp := range f.Imports {
			if imp == "C" {
				continue
			}
			// Map the local import path to a package path.
			if imp1, ok := p.ImportMap[imp]; ok {
				imp = imp1
			}
			if !have[imp] {
				have[imp] = true
				list = append(list, imp)
			}
		}
	}
	sort.Strings(list)
	return list
}

// setBuildIDs computes the build IDs for packages in g.
func (r *Refactor) setBuildIDs(g *pkgGraph) error {
	if debugBuildIDs {
		fmt.Println("start build IDs")
	}
	return g.visitBottomUp(func(p *Package) error {
		if p.BuildID != "" {
			return nil
		}
		if p.PkgPath == "unsafe" {
			p.BuildID = "unsafe"
			return nil
		}
		if !p.InCurrentModule && p.Export != "" {
			p.BuildID = p.ID
			return nil
		}
		h := sha256.New()
		for _, f := range p.Files {
			if !f.Deleted {
				h.Write([]byte(f.Hash))
			}
		}
		for _, imp := range p.Imports {
			pImp := g.byPath(imp)
			h.Write([]byte(imp + "\x00" + pImp.BuildID + "\x00"))
		}
		p.BuildID = fmt.Sprintf("%x", h.Sum(nil))
		if debugBuildIDs {
			fmt.Println("computed BuildID", p.BuildID, "for", p.ID)
		}
		return nil
	})
}

func (s *Snapshot) typeCheck() {
	if debugTypeCheck {
		fmt.Println("start typecheck")
	}
	err := s.pkgGraph.visitBottomUp(func(p *Package) error {
		if debugTypeCheck {
			fmt.Printf("typecheck %s %p for snapshot %p\n", p.ID, p, s)
		}
		s.check(p)
		if p.Types == nil {
			return visitStop
		}
		return nil
	})
	if err != nil {
		s.Errors.Add(err)
	}
}

type snapImporter struct {
	s *Snapshot
	p *Package
}

func (s *snapImporter) Import(importPath string) (*types.Package, error) {
	p := s.s.pkgGraph.byPath(s.p.ImportMap.Lookup(importPath))
	if debugTypeCheck {
		fmt.Printf("import %s %p resolve %s => %s %p\n", s.p.ID, s.p, importPath, p.ID, p)
	}
	if p == nil {
		return nil, fmt.Errorf("import not available: %s", importPath)
	}
	if p.Types == nil {
		// We are running the type-checking in dependency order,
		// so if this happens, it's a mistake.
		return nil, fmt.Errorf("internal error - import not yet available: %s", importPath)
	}
	return p.Types, nil
}

func (s *Snapshot) check(p *Package) {
	if p.PkgPath == "unsafe" {
		p.Types = types.Unsafe
		return
	}

	// If we have the result from a previous snapshot load, use it.
	if c := s.r.cache.types[p.BuildID]; c != nil {
		p.Types = c.pkg
		p.TypesInfo = c.info
		return
	}

	if !p.InCurrentModule && p.Export != "" {
		if s.importer == nil {
			s.importer = importer.ForCompiler(s.fset, "gc", func(path string) (io.ReadCloser, error) {
				p := s.pkgGraph.byPath(path)
				if p == nil || p.Export == "" {
					return nil, fmt.Errorf("no export data for %s", path)
				}
				return os.Open(p.Export)
			})
		}
		tpkg, err := s.importer.Import(p.PkgPath)
		if err != nil {
			s.Errors.Add(err)
		}
		p.Types = tpkg
		s.r.cache.types[p.BuildID] = &cachedTypeInfo{p.Types, nil}
		return
	}

	conf := &types.Config{
		Error:    s.Errors.Add,
		Importer: &snapImporter{s, p},
		Sizes:    s.sizes,
	}
	info := &types.Info{
		Types:      make(map[ast.Expr]types.TypeAndValue),
		Defs:       make(map[*ast.Ident]types.Object),
		Uses:       make(map[*ast.Ident]types.Object),
		Selections: make(map[*ast.SelectorExpr]*types.Selection),
		Scopes:     make(map[ast.Node]*types.Scope),
	}

	var files []*ast.File
	for _, f := range p.Files {
		if !f.Deleted {
			files = append(files, f.Syntax)
		}
	}
	tpkg, err := conf.Check(p.PkgPath, s.fset, files, info)
	if err != nil {
		return
	}
	p.Types = tpkg
	p.TypesInfo = info

	s.r.cache.types[p.BuildID] = &cachedTypeInfo{p.Types, p.TypesInfo}
}

func opener(name string) func(string) (io.ReadCloser, error) {
	return func(ignored string) (io.ReadCloser, error) {
		f, err := os.Open(name)
		if err != nil {
			return nil, err
		}
		return f, nil
	}
}

// Reset undoes any edits to s.
func (s *Snapshot) Reset() {
	for k, edit := range s.edits {
		if edit.Create {
			delete(s.files, edit.Name)
		}
		delete(s.edits, k)
	}
	s.Errors.errs = nil
}

// MergeSnapshots combines all Snapshots. The resulting Snapshot contains only
// files and cannot be type-checked, but can be written out. If any Snapshots
// are inconsistent, it prints details to r.Stderr and returns an error.
//
// TODO: Maybe there should be another type representing just the modified file
// system. Snapshots could use that internally so we can still, e.g., show diffs
// on errors, and this could return it directly rather than returning a weird
// Snapshot. That would make it easy to separate out "apply edits" from "build a
// new Snapshot" in apply, and here we could reuse "apply edits".
func (r *Refactor) MergeSnapshots() (*Snapshot, error) {
	var errs ErrorList
	failed := false
	merge := func(base, s *Snapshot, doEdits bool) {
		// Merge s into base.
		for _, name := range s.fileNames() {
			// Get the file from s.
			f := s.files[name]
			if ed := s.edits[name]; doEdits && ed != nil {
				if ed.Delete {
					f = &File{
						Name:    f.Name,
						Deleted: true,
					}
				} else {
					text, err := ed.NewText()
					if err != nil {
						errs.Add(fmt.Errorf("%s: %v", f.Name, err))
						continue
					}
					f, err = r.cache.newFileText(f.Name, text, true)
					if err != nil {
						// TODO: If we failed with a parse error and are trying
						// to merge snapshots to report the diff, we'll get a
						// second parse error here, which is silly because we
						// really only care about the raw text, not the AST.
						errs.Add(err)
						continue
					}
				}
			}

			f1 := base.files[name]
			if f1 == nil {
				// The file doesn't exist in base, so no conflicts.
				base.files[name] = f
				continue
			}
			if !f.Modified && !f1.Modified {
				// Neither Snapshot modified it. No need to check further.
				continue
			}
			// The file exists in both and was modified in at least one. Check
			// that the contents agree.
			if !bytes.Equal(f.Text, f1.Text) {
				failed = true
				if !filepath.IsAbs(name) {
					name = filepath.Join(r.dir, name)
				}
				rel, err := filepath.Rel(r.modRoot, name)
				if err != nil {
					panic(err)
				}
				fmt.Fprintf(r.Stderr, "%s: refactoring diverged\n", rel)
				d, err := diff.Diff("s1/"+rel, f1.Text, "s2/"+rel, f.Text)
				if err != nil {
					panic(err)
				}
				r.Stderr.Write(d)
			}
		}
	}

	// We merge all of the current Snapshots or r into ms, as well as all of the
	// starting Snapshots of r into base so that ms.Diff has a base Snapshot to
	// diff against.
	base := &Snapshot{
		r:     r,
		files: make(map[string]*File),
	}
	ms := &Snapshot{
		parent: base,
		r:      r,
		files:  make(map[string]*File),
	}
	for _, s := range r.snapshots {
		merge(ms, s, true)

		parent := s
		for parent.parent != nil {
			parent = parent.parent
		}
		merge(base, parent, false)
	}
	if failed {
		return nil, fmt.Errorf("refactoring diverged")
	}
	if err := errs.Err(); err != nil {
		return nil, err
	}
	return ms, nil
}

func (c *buildCache) newFile(name string) (*File, error) {
	if !filepath.IsAbs(name) {
		name = filepath.Join(c.r.dir, name)
	}
	text, err := os.ReadFile(name)
	if err != nil {
		return nil, err
	}
	return c.newFileText(name, text, false)
}

func (c *buildCache) newFileText(name string, text []byte, modified bool) (*File, error) {
	if !filepath.IsAbs(name) {
		name = filepath.Join(c.r.dir, name)
	}
	name = c.r.shortPath(name)

	h := sha256.New()
	h.Write([]byte(name))
	h.Write([]byte("\x00"))
	h.Write(text)
	sum := fmt.Sprintf("%x", h.Sum(nil))

	// If we have the result from a previous parse, use it.
	// This is especially important for test packages, so that
	// they use the same ast.File as the non-test package
	// when files are shared.
	if f := c.files[sum]; f != nil {
		return f, nil
	}

	syntax, err := parser.ParseFile(c.fset, name, text, 0)
	if err != nil {
		return nil, err
	}
	f := &File{
		Name:     name,
		Text:     text,
		Syntax:   syntax,
		Imports:  importsOf(syntax),
		Modified: modified,
		Hash:     sum,
	}
	c.files[sum] = f
	return f, nil
}

func importsOf(file *ast.File) []string {
	var list []string
	for _, spec := range file.Imports {
		path := importPath(spec)
		if path != "" {
			list = append(list, path)
		}
	}
	return list
}

type jsonPackage struct {
	Dir           string      // directory containing package sources
	ImportPath    string      // import path of package in dir
	ImportComment string      // path in import comment on package statement
	Name          string      // package name
	Doc           string      // package documentation string
	Target        string      // install path
	Shlib         string      // the shared library that contains this package (only set when -linkshared)
	Goroot        bool        // is this package in the Go root?
	Standard      bool        // is this package part of the standard Go library?
	Stale         bool        // would 'go install' do anything for this package?
	StaleReason   string      // explanation for Stale==true
	Root          string      // Go root or Go path dir containing this package
	ConflictDir   string      // this directory shadows Dir in $GOPATH
	BinaryOnly    bool        // binary-only package (no longer supported)
	ForTest       string      // package is only for use in named test
	Export        string      // file containing export data (when using -export)
	BuildID       string      // build ID of the compiled package (when using -export)
	Module        *jsonModule // info about package's containing module, if any (can be nil)
	Match         []string    // command-line patterns matching this package
	DepOnly       bool        // package is only a dependency, not explicitly listed

	// Source files
	GoFiles           []string // .go source files (excluding CgoFiles, TestGoFiles, XTestGoFiles)
	CgoFiles          []string // .go source files that import "C"
	CompiledGoFiles   []string // .go files presented to compiler (when using -compiled)
	IgnoredGoFiles    []string // .go source files ignored due to build constraints
	IgnoredOtherFiles []string // non-.go source files ignored due to build constraints
	CFiles            []string // .c source files
	CXXFiles          []string // .cc, .cxx and .cpp source files
	MFiles            []string // .m source files
	HFiles            []string // .h, .hh, .hpp and .hxx source files
	FFiles            []string // .f, .F, .for and .f90 Fortran source files
	SFiles            []string // .s source files
	SwigFiles         []string // .swig files
	SwigCXXFiles      []string // .swigcxx files
	SysoFiles         []string // .syso object files to add to archive
	TestGoFiles       []string // _test.go files in package
	XTestGoFiles      []string // _test.go files outside package

	// Cgo directives
	CgoCFLAGS    []string // cgo: flags for C compiler
	CgoCPPFLAGS  []string // cgo: flags for C preprocessor
	CgoCXXFLAGS  []string // cgo: flags for C++ compiler
	CgoFFLAGS    []string // cgo: flags for Fortran compiler
	CgoLDFLAGS   []string // cgo: flags for linker
	CgoPkgConfig []string // cgo: pkg-config names

	// Dependency information
	Imports      []string          // import paths used by this package
	ImportMap    map[string]string // map from source import to ImportPath (identity entries omitted)
	Deps         []string          // all (recursively) imported dependencies
	TestImports  []string          // imports from TestGoFiles
	XTestImports []string          // imports from XTestGoFiles

	// Error information
	Incomplete bool                // this package or a dependency has an error
	Error      *jsonPackageError   // error loading package
	DepsErrors []*jsonPackageError // errors loading dependencies
}

type jsonPackageError struct {
	ImportStack []string // shortest path from package named on command line to this one
	Pos         string   // position of error (if present, file:line:col)
	Err         string   // the error itself
}

type jsonModule struct {
	Path      string           // module path
	Version   string           // module version
	Versions  []string         // available module versions (with -versions)
	Replace   *jsonModule      // replaced by this module
	Time      *time.Time       // time version was created
	Update    *jsonModule      // available update, if any (with -u)
	Main      bool             // is this the main module?
	Indirect  bool             // is this module only an indirect dependency of main module?
	Dir       string           // directory holding files for this module, if any
	GoMod     string           // path to go.mod file used when loading this module, if any
	GoVersion string           // go version used in module
	Retracted string           // retraction information, if any (with -retracted or -u)
	Error     *jsonModuleError // error loading module
}

type jsonModuleError struct {
	Err string // the error itself
}

// stringList flattens its arguments into a single []string.
// Each argument in args must have type string or []string.
// Copied from cmd/go.
func stringList(args ...interface{}) []string {
	var x []string
	for _, arg := range args {
		switch arg := arg.(type) {
		case []string:
			x = append(x, arg...)
		case string:
			x = append(x, arg)
		default:
			panic("stringList: invalid argument of type " + fmt.Sprintf("%T", arg))
		}
	}
	return x
}
