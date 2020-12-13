// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package refactor

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"sort"
)

// A QualName is a qualified name: a name and its package path.
type QualName struct {
	Pkg  *Package
	Name string
}

func (q QualName) String() string {
	return q.Pkg.PkgPath + "." + q.Name
}

type DepsGraphLevel int

const (
	PkgRefs DepsGraphLevel = iota
	CrossPkgSymRefs
	SymRefs
)

// A DepsGraph records dependency information.
type DepsGraph struct {
	Level DepsGraphLevel
	G     map[*Package]map[string]map[*Package]map[string]bool
}

func (g *DepsGraph) Map(remap map[QualName]QualName) *DepsGraph {
	ng := new(DepsGraph)
	ng.Level = g.Level
	for p1, m := range g.G {
		for name1, m := range m {
			q1 := QualName{p1, name1}
			if q, ok := remap[q1]; ok {
				q1 = q
			}
			for p2, m := range m {
				for name2 := range m {
					q2 := QualName{p2, name2}
					if q, ok := remap[q2]; ok {
						q2 = q
					}
					ng.add(q1, q2)
				}
			}
		}
	}
	return ng
}

func (s *Snapshot) DepsGraph(level DepsGraphLevel) *DepsGraph {
	g := new(DepsGraph)
	g.Level = level
	for _, p := range s.packages {
		s.addPkgDeps(g, p)
	}
	return g
}

func (g *DepsGraph) add(from, to QualName) {
	if from.Name == "" {
		panic("empty from")
	}
	if to.Name == "" {
		panic("empty to")
	}
	if g.Level < SymRefs && from.Pkg == to.Pkg {
		return
	}
	if g.Level == PkgRefs {
		from.Name = ""
		to.Name = ""
	}
	if g.G == nil {
		g.G = make(map[*Package]map[string]map[*Package]map[string]bool)
	}
	m1 := g.G[from.Pkg]
	if m1 == nil {
		m1 = make(map[string]map[*Package]map[string]bool)
		g.G[from.Pkg] = m1
	}
	m2 := m1[from.Name]
	if m2 == nil {
		m2 = make(map[*Package]map[string]bool)
		m1[from.Name] = m2
	}
	m3 := m2[to.Pkg]
	if m3 == nil {
		m3 = make(map[string]bool)
		m2[to.Pkg] = m3
	}
	m3[to.Name] = true
}

func (s *Snapshot) addPkgDeps(g *DepsGraph, p *Package) {
	for _, file := range p.Files {
		if file.Syntax == nil {
			continue
		}
		for _, decl := range file.Syntax.Decls {
			switch decl := decl.(type) {
			case *ast.GenDecl:
				if decl.Tok == token.IMPORT {
					continue
				}
				for _, spec := range decl.Specs {
					switch spec := spec.(type) {
					default:
						panic(fmt.Sprintf("bad spec %T", spec))
					case *ast.ValueSpec:
						for i, id := range spec.Names {
							s.addDeps(g, QualName{p, id.Name}, p, spec.Type)
							if i < len(spec.Values) {
								s.addDeps(g, QualName{p, id.Name}, p, spec.Values[i])
							}
						}
					case *ast.TypeSpec:
						s.addDeps(g, QualName{p, spec.Name.Name}, p, spec.Type)
					}
				}
			case *ast.FuncDecl:
				var name string
				if decl.Recv != nil {
					t := decl.Recv.List[0].Type
					if p, ok := t.(*ast.StarExpr); ok {
						t = p.X
					}
					id, ok := t.(*ast.Ident)
					if !ok {
						panic("lost receiver name")
					}
					name = id.Name + "." + decl.Name.Name
					g.add(QualName{p, id.Name}, QualName{p, name})
					g.add(QualName{p, name}, QualName{p, id.Name})
				} else {
					name = decl.Name.Name
				}
				s.addDeps(g, QualName{p, name}, p, decl.Type)
				s.addDeps(g, QualName{p, name}, p, decl.Body)
			}
		}
	}
}

func (s *Snapshot) addDeps(g *DepsGraph, from QualName, p *Package, n ast.Node) {
	if p == nil {
		panic("NO P")
	}
	if p.TypesInfo == nil {
		panic("NO TYPESINFO")
	}
	Walk(n, func(stack []ast.Node) {
		switch n := stack[0].(type) {
		case *ast.Ident:
			obj := p.TypesInfo.Uses[n]
			if obj == nil {
				return
			}
			tpkg := obj.Pkg()
			if tpkg == nil {
				return
			}
			if _, ok := obj.(*types.PkgName); ok {
				return
			}
			if obj.Parent() != tpkg.Scope() {
				return
			}
			for _, pp := range s.packages {
				if pp.Types == tpkg {
					g.add(from, QualName{pp, obj.Name()})
					break
				}
			}
		}
	})
}

func (s *Snapshot) CheckImportCycle(g *DepsGraph) error {
	// Find import cycle (at least one).
	walked := make(map[*Package]int)
	var stack []*Package
	var cycle []*Package
	var walk func(*Package)
	walk = func(p *Package) {
		if walked[p] == 2 || cycle != nil {
			return
		}
		if walked[p] == 1 {
			// cycle!
			for i := len(stack) - 1; i >= 0; i-- {
				if stack[i] == p {
					cycle = append(cycle, stack[i:]...)
					return
				}
			}
			return
		}
		walked[p] = 1
		stack = append(stack, p)
		for _, m := range g.G[p] {
			for p1 := range m {
				if p1 != p {
					walk(p1)
				}
			}
		}
		stack = stack[:len(stack)-1]
		walked[p] = 2
	}
	for p := range g.G {
		walk(p)
	}
	if cycle == nil {
		return nil
	}

	var b bytes.Buffer
	off := 0
	for i := range cycle {
		if cycle[i].PkgPath < cycle[off].PkgPath {
			off = i
		}
	}
	fmt.Fprintf(&b, "%s", cycle[off].PkgPath)
	for i := 1; i <= len(cycle); i++ {
		fmt.Fprintf(&b, " -> %s", cycle[(off+i)%len(cycle)].PkgPath)
	}

	if g.Level >= CrossPkgSymRefs {
		from := cycle[off]
		for i := 1; i <= len(cycle); i++ {
			fmt.Fprintf(&b, "\n# %s", from.PkgPath)
			to := cycle[(off+i)%len(cycle)]
			var syms []string
			for sym, m := range g.G[from] {
				if m[to] != nil {
					syms = append(syms, sym)
				}
			}
			sort.Strings(syms)
			count := 0
			for _, sym := range syms {
				for name := range g.G[from][sym][to] {
					if count < 20 {
						fmt.Fprintf(&b, "\n%s.%s -> %s.%s", from.Name, sym, to.Name, name)
					}
					count++
				}
			}
			if count > 20 {
				fmt.Fprintf(&b, "\n ... and %d more", count-20)
			}
			from = to
		}
	}

	return fmt.Errorf("import cycle: %v", b.String())
}
