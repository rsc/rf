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
	"go/token"
	"go/types"
	"sort"
	"strconv"
	"strings"
)

func deleteUnusedImports(s *Snapshot, p *Package, file *ast.File) {
	used := make(map[string]bool)
	Walk(file, func(stack []ast.Node) {
		if id, ok := stack[0].(*ast.Ident); ok && id.Obj == nil {
			if _, ok := stack[1].(*ast.SelectorExpr); !ok {
				return
			}
			used[id.Name] = true
		}
	})

	deleteImports(s.fset, file, func(name, pkg string) bool {
		if name == "" {
			p1 := s.pkgByID[s.importToID(p, pkg)]
			if p1 == nil {
				panic("NO IMPORT: " + pkg)
			}
			name = p1.Name
		}
		return !used[name]
	})
}

// deleteImports deletes all imports matching match.
func deleteImports(fset *token.FileSet, f *ast.File, match func(name, path string) bool) {
	var delspecs []*ast.ImportSpec
	var delcomments []*ast.CommentGroup

	// Find the import nodes that import path, if any.
	for i := 0; i < len(f.Decls); i++ {
		decl := f.Decls[i]
		gen, ok := decl.(*ast.GenDecl)
		if !ok || gen.Tok != token.IMPORT {
			continue
		}
		for j := 0; j < len(gen.Specs); j++ {
			spec := gen.Specs[j]
			impspec := spec.(*ast.ImportSpec)
			if !match(importName(impspec), importPath(impspec)) {
				continue
			}

			// We found an import spec that imports path.
			// Delete it.
			delspecs = append(delspecs, impspec)
			copy(gen.Specs[j:], gen.Specs[j+1:])
			gen.Specs = gen.Specs[:len(gen.Specs)-1]

			// If this was the last import spec in this decl,
			// delete the decl, too.
			if len(gen.Specs) == 0 {
				copy(f.Decls[i:], f.Decls[i+1:])
				f.Decls = f.Decls[:len(f.Decls)-1]
				i--
				break
			} else if len(gen.Specs) == 1 {
				if impspec.Doc != nil {
					delcomments = append(delcomments, impspec.Doc)
				}
				if impspec.Comment != nil {
					delcomments = append(delcomments, impspec.Comment)
				}
				for _, cg := range f.Comments {
					// Found comment on the same line as the import spec.
					if cg.End() < impspec.Pos() && fset.Position(cg.End()).Line == fset.Position(impspec.Pos()).Line {
						delcomments = append(delcomments, cg)
						break
					}
				}

				spec := gen.Specs[0].(*ast.ImportSpec)

				// Move the documentation right after the import decl.
				if spec.Doc != nil {
					for fset.Position(gen.TokPos).Line+1 < fset.Position(spec.Doc.Pos()).Line {
						fset.File(gen.TokPos).MergeLine(fset.Position(gen.TokPos).Line)
					}
				}
				for _, cg := range f.Comments {
					if cg.End() < spec.Pos() && fset.Position(cg.End()).Line == fset.Position(spec.Pos()).Line {
						for fset.Position(gen.TokPos).Line+1 < fset.Position(spec.Pos()).Line {
							fset.File(gen.TokPos).MergeLine(fset.Position(gen.TokPos).Line)
						}
						break
					}
				}
			}
			if j > 0 {
				lastImpspec := gen.Specs[j-1].(*ast.ImportSpec)
				lastLine := fset.Position(lastImpspec.Path.ValuePos).Line
				line := fset.Position(impspec.Path.ValuePos).Line

				// We deleted an entry but now there may be
				// a blank line-sized hole where the import was.
				if line-lastLine > 1 || !gen.Rparen.IsValid() {
					// There was a blank line immediately preceding the deleted import,
					// so there's no need to close the hole. The right parenthesis is
					// invalid after AddImport to an import statement without parenthesis.
					// Do nothing.
				} else if line != fset.File(gen.Rparen).LineCount() {
					// There was no blank line. Close the hole.
					fset.File(gen.Rparen).MergeLine(line)
				}
			}
			j--
		}
	}

	// Delete imports from f.Imports.
	for i := 0; i < len(f.Imports); i++ {
		imp := f.Imports[i]
		for j, del := range delspecs {
			if imp == del {
				copy(f.Imports[i:], f.Imports[i+1:])
				f.Imports = f.Imports[:len(f.Imports)-1]
				copy(delspecs[j:], delspecs[j+1:])
				delspecs = delspecs[:len(delspecs)-1]
				i--
				break
			}
		}
	}

	// Delete comments from f.Comments.
	for i := 0; i < len(f.Comments); i++ {
		cg := f.Comments[i]
		for j, del := range delcomments {
			if cg == del {
				copy(f.Comments[i:], f.Comments[i+1:])
				f.Comments = f.Comments[:len(f.Comments)-1]
				copy(delcomments[j:], delcomments[j+1:])
				delcomments = delcomments[:len(delcomments)-1]
				i--
				break
			}
		}
	}

	if len(delspecs) > 0 {
		panic(fmt.Sprintf("deleted specs from Decls but not Imports: %v", delspecs))
	}

	return
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
		panic(file)
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
