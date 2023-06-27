// Copyright 2022 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package refactor

import (
	"bytes"
	"fmt"
	"io"
	"sort"
)

// A pkgGraph is a package dependency graph used during loading. While a single
// package can appear in multiple variants, a given pkgGraph only represents
// each package path as one node, and loading builds multiple graphs for
// different package variants.
type pkgGraph struct {
	name      string              // name for debugging
	pkgByPath map[string]*Package // keyed by Package.PkgPath
	nEdges    int                 // total number of edges
}

func newPkgGraph(name string) *pkgGraph {
	return &pkgGraph{name: name, pkgByPath: make(map[string]*Package)}
}

func (g *pkgGraph) add(p *Package) {
	if o := g.pkgByPath[p.PkgPath]; o != nil {
		panic(fmt.Sprintf("duplicate package path %q in %q and %q", p.PkgPath, p.ID, o.ID))
	}
	g.pkgByPath[p.PkgPath] = p
	g.nEdges += len(p.Imports)
}

func (g *pkgGraph) byPath(pkgPath string) *Package {
	return g.pkgByPath[pkgPath]
}

func (g *pkgGraph) packages() []*Package {
	var list []*Package
	for _, p := range g.pkgByPath {
		list = append(list, p)
	}
	sort.Slice(list, func(i, j int) bool {
		return list[i].PkgPath < list[j].PkgPath
	})
	return list
}

// merge merges graphs g and g2 into a new package graph. For any Package that
// appears in both g and g2, one must be a superset of the other, for both
// imports and files. (This holds true for our uses and allows merge to keep one
// node or the other without synthesizing a new node).
func (g *pkgGraph) merge(g2 *pkgGraph) *pkgGraph {
	g3 := newPkgGraph("merge")
	// Clone g.
	for pkgPath, pkg := range g.pkgByPath {
		g3.pkgByPath[pkgPath] = pkg
	}
	g3.nEdges = g.nEdges
	// Add nodes from g2.
	imports := make(map[string]bool)
	files := make(map[string]bool)
	isSuperset := func(pkg1, pkg2 *Package) bool {
		for k := range imports {
			delete(imports, k)
		}
		for _, imp := range pkg1.Imports {
			imports[imp] = true
		}
		for _, imp := range pkg2.Imports {
			if !imports[imp] {
				return false
			}
		}
		for k := range files {
			delete(files, k)
		}
		for _, f := range pkg1.Files {
			files[f.Name] = true
		}
		for _, f := range pkg2.Files {
			if !files[f.Name] {
				return false
			}
		}
		return true
	}
	for pkgPath, pkg2 := range g2.pkgByPath {
		if pkg, ok := g.pkgByPath[pkgPath]; !ok {
			// This node isn't present. Simply add it.
			g3.pkgByPath[pkgPath] = pkg2
			g3.nEdges += len(pkg2.Imports)
		} else if isSuperset(pkg2, pkg) {
			// The node from g2 is a supserset of the node from g, so that take
			// node instead.
			g3.pkgByPath[pkgPath] = pkg2
			g3.nEdges += len(pkg2.Imports) - len(pkg.Imports)
		} else if !isSuperset(pkg, pkg2) {
			// Neither node is a supserset of the other. This shouldn't happen.
			fileNames := func(p *Package) []string {
				var o []string
				for _, f := range p.Files {
					o = append(o, f.Name)
				}
				return o
			}
			panic(fmt.Sprintf("package %q (imports %q files %q) is disjoint from package %q (imports %q files %q)", pkg.ID, pkg.Imports, fileNames(pkg), pkg2.ID, pkg2.Imports, fileNames(pkg2)))
		}
	}
	return g3
}

// clonePackages creates a copy of g and all Packages in g.
func (g *pkgGraph) clonePackages() *pkgGraph {
	g1 := newPkgGraph(g.name)
	for pkgPath, pkg := range g.pkgByPath {
		pkg1 := &Package{
			Name:            pkg.Name,
			Dir:             pkg.Dir,
			ID:              pkg.ID,
			PkgPath:         pkg.PkgPath,
			ForTest:         pkg.ForTest,
			Files:           append([]*File(nil), pkg.Files...),
			Imports:         append([]string(nil), pkg.Imports...),
			InCurrentModule: pkg.InCurrentModule,
			Export:          pkg.Export,
			// Don't copy BuildID because it's per-graph.
			// ImportMap is immutable, so no need to clone.
			ImportMap: pkg.ImportMap,
			// Don't copy type-checking info because it's per-graph.
		}
		g1.pkgByPath[pkgPath] = pkg1
	}
	g1.nEdges = g.nEdges
	return g1
}

// findCycle finds some cycle in g. If there is a cycle, t returns a non-nil
// pkgCycle. If diagnose is true, it populates pkgCycle with the cycle found.
func (g *pkgGraph) findCycle(diagnose bool) *pkgCycle {
	walked := make(map[*Package]int8)
	var stack []*Package
	var cycle []*Package
	var walk func(*Package) bool
	walk = func(p *Package) bool {
		if walked[p] == 2 {
			return false
		}
		if walked[p] == 1 {
			// Found a cycle.
			if diagnose {
				for i := len(stack) - 1; i >= 0; i-- {
					if stack[i] == p {
						cycle = append(cycle, stack[i:]...)
						break
					}
				}
			}
			return true
		}
		walked[p] = 1
		if diagnose {
			stack = append(stack, p)
		}
		for _, imp := range p.Imports {
			p1 := g.pkgByPath[imp]
			if p1 == nil {
				panic(fmt.Sprintf("package %q imports %q, which is missing from the package graph", p.ID, imp))
			}
			if walk(p1) {
				return true
			}
		}
		if diagnose {
			stack = stack[:len(stack)-1]
		}
		walked[p] = 2
		return false
	}
	for _, p := range g.pkgByPath {
		if walk(p) {
			if !diagnose {
				return &pkgCycle{}
			}

			// Rotate the cycle into some canonical order. (If there's more than
			// one cycle, this isn't enough to make this whole function
			// deterministic, but it helps.)
			off := 0
			for i := range cycle {
				if cycle[i].ID < cycle[off].ID {
					off = i
				}
			}
			var c pkgCycle
			for i := range cycle {
				c.pkgs = append(c.pkgs, cycle[(off+i)%len(cycle)])
			}
			return &c
		}
	}
	return nil
}

type pkgCycle struct {
	pkgs []*Package
}

func (c *pkgCycle) String() string {
	if len(c.pkgs) == 0 {
		return "<cycle>"
	}
	var b bytes.Buffer
	for _, pkg := range c.pkgs {
		b.WriteString(pkg.ID)
		b.WriteString(" -> ")
	}
	b.WriteString(c.pkgs[0].ID) // Intentionally repeated
	return b.String()
}

type errVisitStop struct{}

func (errVisitStop) Error() string { return "visit stop" }

var visitStop errVisitStop

func (g *pkgGraph) visitBottomUp(visit func(p *Package) error) error {
	// Diagnose cycles (well, at least one).
	if c := g.findCycle(true); c != nil {
		return fmt.Errorf("import cycle: %s", c)
	}

	// Build dependency graph.
	var errs ErrorList
	ready := make(map[*Package]bool)
	waiting := make(map[*Package]map[*Package]bool)
	rdeps := make(map[*Package][]*Package)
	for _, p := range g.packages() {
		if len(p.Imports) == 0 {
			ready[p] = true
		} else {
			waiting[p] = make(map[*Package]bool)
			for _, pkgPath := range p.Imports {
				p1 := g.byPath(pkgPath)
				if p1 == nil {
					errs.Add(fmt.Errorf("package %s imports %s, but %s isn't in the package graph", p, pkgPath, pkgPath))
					continue
				}
				waiting[p][p1] = true
				rdeps[p1] = append(rdeps[p1], p)
			}
		}
	}
	if err := errs.Err(); err != nil {
		return err
	}

	// Visit.
	stopped := false
	for len(ready) > 0 {
		for p := range ready {
			err := visit(p)
			delete(ready, p)
			if err != nil {
				// Visit failed - do not wake reverse dependencies, but do keep
				// visiting other packages.
				stopped = true
				if err != visitStop {
					errs.Add(err)
				}
				continue
			}
			for _, p1 := range rdeps[p] {
				delete(waiting[p1], p)
				if len(waiting[p1]) == 0 {
					delete(waiting, p1)
					ready[p1] = true
				}
			}
		}
	}

	// Self-check.
	if len(waiting) > 0 && !stopped {
		fmt.Println("visit stalled:")
		for p, n := range waiting {
			fmt.Println(p.PkgPath, n, rdeps[p])
		}
		panic("visit did not complete")
	}

	return errs.Err()
}

func (g *pkgGraph) dump(w io.Writer) {
	pkgs := g.packages()
	for _, pkg := range pkgs {
		fmt.Fprintf(w, "package %s (ID %s)\n", pkg.PkgPath, pkg.ID)
		for _, imp := range pkg.Imports {
			fmt.Fprintf(w, "  import %s\n", imp)
		}
		for _, f := range pkg.Files {
			fmt.Fprintf(w, "  file %s\n", f.Name)
		}
	}
}
