// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"path/filepath"

	"golang.org/x/tools/go/packages"
	"rsc.io/rf/refactor"
)

// transplant rewrites code, which is from src, for insertion at dst.
// Along the way it updates any references in moves to use the new location.
func transplant(snap *refactor.Snapshot, code string, src, dst token.Pos, moves map[types.Object]*packages.Package) string {
	srcPkg, srcFile := snap.FileAt(src)
	if srcFile == nil {
		panic("lost source file")
	}
	dstPkg := snap.PackageAt(dst)

	ed := refactor.NewBufferAt(src, []byte(code))
	refactor.WalkRange(srcFile, src, src+token.Pos(len(code)), func(stack []ast.Node) {
		id, ok := stack[0].(*ast.Ident)
		if !ok {
			return
		}
		_, havePkgDot := stack[1].(*ast.SelectorExpr)

		obj := srcPkg.TypesInfo.Uses[id]
		if obj == nil {
			return
		}
		objPkg := obj.Pkg()
		if objPkg == nil {
			return
		}

		if obj.Parent() != objPkg.Scope() {
			// Not a package-scope variable.
			return
		}
		if mp := moves[obj]; mp != nil {
			objPkg = mp.Types
		}

		if havePkgDot {
			ed.Delete(stack[1].Pos(), id.Pos())
		}
		if objPkg == dstPkg.Types {
			// Need to remove qualifier if any, make sure name is not shadowed.
			// (For moving globals, shadowed would mean a duplicate definition,
			// which should be caught earlier but may not be.)
			if found := snap.LookupAt(id.Name, dst); found != nil && found != obj {
				snap.ErrorAt(dst, "%s is already declared\n\t%s: previous declaration", id.Name, snap.Addr(found.Pos()))
			}
		} else {
			snap.NeedImport(dst, objPkg.Name(), objPkg)
			ed.Insert(id.Pos(), objPkg.Name()+".")
		}
	})
	return ed.String()
}

// mvCode moves the code described by srcs into dst.
func mvCode(snap *refactor.Snapshot, srcs []*refactor.Item, dst *refactor.Item, dstPkg *packages.Package) {
	// Build list of what's moving.
	// Keep a list of the objects in files separately, so that we can avoid moving them twice.
	moves := make(map[types.Object]*packages.Package)
	inFiles := make(map[types.Object]*packages.Package)
	for _, src := range srcs {
		if src.Obj != nil {
			for _, obj := range codeObjs(snap, src.Obj) {
				moves[obj] = dstPkg
			}
			continue
		}
		switch src.Kind {
		default:
			panic(fmt.Sprintf("unexpected src %v", src))
		case refactor.ItemFile:
			srcPkg := snap.Targets()[0] // TODO
			srcFile := findFile(snap, srcPkg, src.Name)
			if srcFile == nil {
				snap.ErrorAt(token.NoPos, "cannot find file %s", src.Name)
				continue
			}
			recordFileMoves(srcPkg, srcFile, dstPkg, inFiles)
		case refactor.ItemDir:
			panic("mv dir not implemented")
		}
	}
	for obj, dst := range inFiles {
		moves[obj] = dst
	}

	done := make(map[types.Object]bool)
	for _, src := range srcs {
		if inFiles[src.Obj] != nil || done[src.Obj] {
			continue
		}
		if src.Obj != nil {
			srcPkg, srcFile := snap.FileAt(src.Obj.Pos())
			pos, end := codeRange(snap, src.Obj)
			moveCode(snap, srcPkg, srcFile, pos, end, dst, dstPkg, moves)
		}
		switch src.Kind {
		case refactor.ItemFile:
			srcPkg := snap.Targets()[0] // TODO
			srcFile := findFile(snap, srcPkg, src.Name)
			moveCode(snap, srcPkg, srcFile, 0, 0, dst, dstPkg, moves)
		case refactor.ItemDir:
			panic("mv dir not implemented")
		}
	}

	rewritePkgRefs(snap, moves)
}

func recordFileMoves(srcPkg *packages.Package, file *ast.File, dstPkg *packages.Package, moves map[types.Object]*packages.Package) {
	for _, d := range file.Decls {
		switch d := d.(type) {
		default:
			panic(fmt.Sprintf("unexpected decl %T", d))
		case *ast.GenDecl:
			for _, spec := range d.Specs {
				switch spec := spec.(type) {
				default:
					panic(fmt.Sprintf("unexpected spec %T", spec))
				case *ast.ImportSpec:
					// ignore
				case *ast.ValueSpec:
					for _, id := range spec.Names {
						obj := srcPkg.TypesInfo.Defs[id]
						if obj == nil {
							panic("no obj for var/const")
						}
						moves[obj] = dstPkg
					}
				case *ast.TypeSpec:
					obj := srcPkg.TypesInfo.Defs[spec.Name]
					if obj == nil {
						panic("no obj for type")
					}
					moves[obj] = dstPkg
				}
			}
		case *ast.FuncDecl:
			if d.Recv != nil {
				continue
			}
			obj := srcPkg.TypesInfo.Defs[d.Name]
			if obj == nil {
				panic("no obj for func")
			}
			moves[obj] = dstPkg
		}
	}
}

func moveCode(snap *refactor.Snapshot,
	srcPkg *packages.Package, srcFile *ast.File,
	srcPos, srcEnd token.Pos,
	dst *refactor.Item, dstPkg *packages.Package,
	moves map[types.Object]*packages.Package) {
	// Decide destination file.
	if dst.Kind == refactor.ItemDir {
		// Reduce to file case.
		dst0 := *dst
		dst = &dst0
		dst.Kind = refactor.ItemFile
		dst.Name = filepath.Base(snap.Position(srcFile.Package).Filename)
	}
	if dst.Kind != refactor.ItemFile {
		panic("moveFile")
	}

	srcSplit := srcFile.Name.End() // just after package declaration
	srcHdrEnd := srcFile.Name.End()
	if len(srcFile.Imports) > 0 {
		srcHdrEnd = srcFile.Decls[0].Pos()
		for i := 0; i < len(srcFile.Decls) && isImportDecl(srcFile.Decls[i]); i++ {
			srcSplit = srcFile.Decls[i].End()
		}
	}
	srcStart, srcEOF := snap.FileRange(srcFile.Package)
	if srcPos == token.NoPos {
		srcPos = srcSplit
		srcEnd = srcEOF
	}

	var dstFile *ast.File
	for _, file := range dstPkg.Syntax {
		if filepath.Base(snap.Position(file.Package).Filename) == dst.Name {
			dstFile = file
			break
		}
	}
	if dstFile == nil {
		text := string(snap.Text(srcStart, srcHdrEnd))
		text = text[:srcFile.Name.Pos()-srcStart] + dstPkg.Types.Name() + text[srcFile.Name.End()-srcStart:]
		dstFile = snap.CreateFile(dstPkg, dst.Name, text)
	}
	_, dstPos := snap.FileRange(dstFile.Package)

	snap.InsertAt(dstPos, "\n\n"+
		transplant(snap, string(snap.Text(srcPos, srcEnd)), srcPos, dstPos, moves))

	if srcPos == srcSplit && srcEnd == srcEOF {
		snap.DeleteFile(srcPos)
	} else {
		snap.DeleteAt(srcPos, srcEnd)
	}
}

func rewritePkgRefs(snap *refactor.Snapshot, moves map[types.Object]*packages.Package) {
	snap.ForEachFile(func(pkg *packages.Package, file *ast.File) {
		refactor.Walk(file, func(stack []ast.Node) {
			id, ok := stack[0].(*ast.Ident)
			if !ok {
				return
			}
			obj := pkg.TypesInfo.Uses[id]
			newPkg := moves[obj]
			if newPkg == nil || newPkg.Types == obj.Pkg() {
				return
			}

			// obj is moving to a new package.
			if sel, ok := stack[1].(*ast.SelectorExpr); ok {
				// obj is already otherPkg.Name; update to newPkg.Name.
				if newPkg == pkg {
					// Delete the no-longer-needed package qualifier.
					if xobj := snap.LookupAt(id.Name, id.Pos()); xobj != nil && xobj != obj {
						snap.ErrorAt(id.Pos(), "%s is shadowed at new unqualified use", id.Name)
					}
					snap.DeleteAt(sel.Pos(), id.Pos())
				} else {
					snap.NeedImport(id.Pos(), newPkg.Types.Name(), newPkg.Types)
					snap.ReplaceNode(sel.X, newPkg.Types.Name())
				}
			} else if newPkg == pkg {
				// obj is moving to pkg but is already referred to in pkg without a qualifier.
				// Can happen if pkg imports the old location with a dot import.
				// Can't be shadowed or the reference wouldn't be to obj.
				// Do nothing.
			} else {
				// obj is moving to a new package and needs a qualified import.
				name := snap.NeedImport(id.Pos(), "", newPkg.Types)
				snap.InsertAt(id.Pos(), name+".")
			}
		})
	})
}

func isImportDecl(decl ast.Decl) bool {
	d, ok := decl.(*ast.GenDecl)
	return ok && d.Tok == token.IMPORT
}

func findFile(snap *refactor.Snapshot, pkg *packages.Package, name string) *ast.File {
	for _, file := range pkg.Syntax {
		if filepath.Base(snap.Position(file.Package).Filename) == name {
			return file
		}
	}
	return nil
}

func codeRange(snap *refactor.Snapshot, obj types.Object) (pos, end token.Pos) {
	_, srcFile := snap.FileAt(obj.Pos())
	startFile, endFile := snap.FileRange(obj.Pos())
	text := snap.Text(startFile, endFile)
	pos = srcFile.Name.End()
	if int(pos-startFile) < len(text) && text[pos-startFile] == '\n' {
		pos++
	}
	d := codeDecl(snap, obj)
	for i := 0; i < len(srcFile.Decls); i++ {
		if srcFile.Decls[i] == d {
			break
		}
		pos = srcFile.Decls[i].End()
		if int(pos-startFile) < len(text) && text[pos-startFile] == '\n' {
			pos++
		}
	}
	end = d.End()
	if int(end-startFile) < len(text) && text[end-startFile] == '\n' {
		end++
	}
	return pos, end
}

func codeObjs(snap *refactor.Snapshot, obj types.Object) []types.Object {
	srcPkg, _ := snap.FileAt(obj.Pos())
	defs := srcPkg.TypesInfo.Defs
	var objs []types.Object
	switch d := codeDecl(snap, obj).(type) {
	default:
		panic(fmt.Sprintf("unexpected codeDecl %T", d))
	case *ast.GenDecl:
		for _, spec := range d.Specs {
			switch spec := spec.(type) {
			case *ast.ValueSpec:
				for _, id := range spec.Names {
					objs = append(objs, defs[id])
				}
			case *ast.TypeSpec:
				objs = append(objs, defs[spec.Name])
			}
		}
	case *ast.FuncDecl:
		objs = append(objs, defs[d.Name])
	}
	return objs
}

func codeDecl(snap *refactor.Snapshot, obj types.Object) ast.Decl {
	stack := snap.SyntaxAt(obj.Pos())
	for i := 0; i < len(stack); i++ {
		switch d := stack[i].(type) {
		case *ast.GenDecl:
			for _, spec := range d.Specs {
				switch spec := spec.(type) {
				case *ast.ValueSpec:
					for _, id := range spec.Names {
						if id.Pos() == obj.Pos() {
							return d
						}
					}
				case *ast.TypeSpec:
					if spec.Name.Pos() == obj.Pos() {
						return d
					}
				}
			}
			panic("unexpected decl")
		case *ast.FuncDecl:
			if d.Name.Pos() == obj.Pos() {
				return d
			}
			panic("unexpected func")
		}
	}
	panic("cannot find decl")
}
