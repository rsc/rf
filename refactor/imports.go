// Copyright 2013 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Adapted from golang.org/x/tools/go/ast/astutil/imports.go
// and from gofix's import insertion code.

package refactor

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"go/types"
	"sort"
	"strconv"
	"strings"
)

func deleteUnusedImports(s *Snapshot, p *Package, text []byte) []byte {
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "out.go", text, parser.ParseComments)
	if err != nil {
		return text
	}

	used := make(map[string]bool)
	Walk(file, func(stack []ast.Node) {
		if id, ok := stack[0].(*ast.Ident); ok && id.Obj == nil {
			if _, ok := stack[1].(*ast.SelectorExpr); !ok {
				return
			}
			used[id.Name] = true
		}
	})

	match := func(name, pkg string) bool {
		if name == "" {
			var p1 *Package
			if path, ok := p.ImportMap[pkg]; ok {
				p1 = s.pkgGraph.byPath(path)
			}
			if p1 == nil {
				p1 = s.pkgGraph.byPath(pkg)
			}
			if p1 == nil {
				panic("NO IMPORT: " + pkg)
			}
			name = p1.Name
		}
		return !used[name]
	}

	buf := NewBufferAt(s, 1, text)
	for _, decl := range file.Decls {
		gen, ok := decl.(*ast.GenDecl)
		if !ok || gen.Tok != token.IMPORT {
			continue
		}

		complete := true
		any := false
		for _, spec := range gen.Specs {
			spec := spec.(*ast.ImportSpec)
			if !match(importName(spec), importPath(spec)) {
				complete = false
				continue
			}
			any = true
		}
		if complete {
			buf.Delete(nodeRange(decl, text))
			continue
		}
		if !any {
			continue
		}

		for _, spec := range gen.Specs {
			spec := spec.(*ast.ImportSpec)
			if match(importName(spec), importPath(spec)) {
				buf.Delete(nodeRange(spec, text))
			}
		}
	}
	return buf.Bytes()
}

var (
	slashSlash = []byte("//")
	starSlash  = []byte("*/")
)

func nodeRange(n ast.Node, text []byte) (pos, end token.Pos) {
	startFile, endFile := token.Pos(1), token.Pos(1+len(text))

	pos = n.Pos()
	end = n.End()

	// Include space and comments following the node.
	for end < endFile && text[end-startFile] == ' ' {
		end++
	}
	if bytes.HasPrefix(text[end-startFile:], slashSlash) {
		i := bytes.IndexByte(text[end-startFile:], '\n')
		if i >= 0 {
			end += token.Pos(i)
		} else {
			end = endFile
		}
	}
	if end > n.End() && end < endFile && text[end-startFile] != '\n' {
		// If we consumed spaces but did not reach a newline,
		// put a space back to avoid joining tokens.
		end--
	}

	// Include tabs preceding the node, to beginning of line.
	// (If there are spaces before the node, it means something else
	// precedes the node on the line, so don't bother removing anything.)
	for pos > startFile && text[pos-startFile-1] == '\t' {
		pos--
	}

	// Include comments "attached" to this node,
	// but stopping at a blank line.
	// Reading comments backward is a bit tricky:
	// if we see a */, we need to stop and assume
	// we don't know the state of the world.
	for pos > startFile && text[pos-startFile-1] == '\n' {
		i := bytes.LastIndexByte(text[:pos-startFile-1], '\n') + 1
		line := text[i : pos-startFile]
		line = bytes.TrimSpace(line)
		if !bytes.HasPrefix(line, slashSlash) || bytes.Contains(line, starSlash) {
			break
		}
		pos = startFile + token.Pos(i)
	}

	// Consume final \n if we are deleting the whole line.
	if (pos == startFile || text[pos-startFile-1] == '\n') && end < endFile && text[end-startFile] == '\n' {
		end++
	}

	return pos, end
}

// importName returns the name of s,
// or "" if the import is not named.
func importName(s *ast.ImportSpec) string {
	if s.Name == nil {
		return ""
	}
	return s.Name.Name
}

// importPath returns the unquoted import path of s,
// or "" if the path is not properly quoted.
func importPath(s *ast.ImportSpec) string {
	t, err := strconv.Unquote(s.Path.Value)
	if err != nil {
		return ""
	}
	return t
}

func (s *Snapshot) NeedImport(pos token.Pos, id string, pkg *types.Package) string {
	_, file := s.FileAt(pos)
	if file == nil {
		fmt.Println(s.Position(pos))
		panic("no file")
	}

	want := id
	if id == "" {
		want = pkg.Name()
	}
	names := []string{want, want + "pkg", want + "_"}
	for _, id := range names {
		for _, imp := range file.Imports {
			if importPath(imp) == pkg.Path() {
				name := importName(imp)
				if name == "" {
					name = pkg.Name()
				}
				if name == id {
					if obj := s.LookupAt(name, pos); obj == nil {
						return name
					} else if obj, ok := obj.(*types.PkgName); ok && obj.Pkg().Path() != pkg.Path() {
						return name
					}
				}
			}
		}
	}

	want = ""
	for _, id := range names {
		if obj := s.LookupAt(id, pos); obj == nil {
			want = id
			break
		} else if obj, ok := obj.(*types.PkgName); ok && obj.Pkg().Path() != pkg.Path() {
			want = id
			break
		}
	}
	if want == "" {
		s.ErrorAt(pos, "package name %s is shadowed", pkg.Name())
		want = pkg.Name()
	}

	ed := s.editAt(file.Package)
	key := NewImport{want, pkg}
	for _, p := range ed.AddImports {
		if p == key {
			return want
		}
	}
	ed.AddImports = append(ed.AddImports, key)
	return want
}

func (s *Snapshot) addImports() {
	for file, ed := range s.edits {
		s.addImportList(file, ed.AddImports)
	}
}

func (s *Snapshot) addImportList(file string, list []NewImport) {
	_, f := s.FileByName(file)
	if f == nil {
		return
	}
	imps := f.Decls
	for i, d := range f.Decls {
		if d, ok := d.(*ast.GenDecl); !ok || d.Tok != token.IMPORT {
			imps = f.Decls[:i]
			break
		}
	}

	// Assign each import to an import statement.
	needs := make(map[*ast.ImportSpec][]NewImport)
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
				s.InsertAt(spec.Pos(), fmt.Sprintf("%s %q\n", id, need.pkg.Path()))
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
