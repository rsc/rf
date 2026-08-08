// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"path"
	"strings"

	"rsc.io/rf/refactor"
)

func cmdAdd(snap *refactor.Snapshot, args string) error {
	return cmdAddSub(snap, "add", args)
}

func cmdSub(snap *refactor.Snapshot, args string) error {
	return cmdAddSub(snap, "sub", args)
}

func parseImportFlags(cmd, args string) ([]string, string, error) {
	var imports []string
	for {
		flag, rest, _ := cutAny(strings.TrimLeft(args, " \t\n"), " \t\n")
		if flag != "-i" {
			break
		}
		rest = strings.TrimLeft(rest, " \t\n")
		if rest == "" {
			return nil, "", newErrUsage("%s -i requires an argument", cmd)
		}
		var imp string
		if q := rest[0]; q == '"' || q == '\'' || q == '`' {
			var ok bool
			imp, rest, ok = cut(rest[1:], string(q))
			if !ok {
				return nil, "", newErrUsage("%s unterminated quote in -i argument", cmd)
			}
		} else {
			imp, rest, _ = cutAny(rest, " \t\n")
		}
		if imp == "" {
			return nil, "", newErrUsage("%s -i requires non-empty argument", cmd)
		}
		imports = append(imports, imp)
		args = rest
	}
	return imports, args, nil
}

func cmdAddSub(snap *refactor.Snapshot, cmd, args string) error {
	imports, args, err := parseImportFlags(cmd, args)
	if err != nil {
		return err
	}

	item, expr, text := snap.EvalNext(args)
	if expr == "" {
		return newErrUsage("%s address text...", cmd)
	}
	if item == nil {
		// Error already reported.
		return nil
	}

	var pos, end token.Pos
	switch item.Kind {
	default:
		return fmt.Errorf("TODO: %s after %s", cmd, item.Kind)

	case refactor.ItemNotFound:
		return newErrPrecondition("%s not found", item.Name)

	case refactor.ItemConst, refactor.ItemFunc, refactor.ItemType, refactor.ItemVar, refactor.ItemField:
		stack := snap.SyntaxAt(item.Obj.Pos())
		if len(stack) == 0 {
			panic("LOST " + item.Name)
		}
		after := stack[1]
		switch after.(type) {
		case *ast.ValueSpec, *ast.TypeSpec:
			decl := stack[2].(*ast.GenDecl)
			if decl.Lparen == token.NoPos {
				after = decl
			}
		}
		pos, end = nodeRange(snap, after)

	case refactor.ItemFile:
		_, srcFile := snap.FileByName(item.Name)
		pos, end = snap.FileRange(srcFile.Package)

	case refactor.ItemDir:
		var dstPkg *refactor.Package
		for _, pkg := range snap.Packages() {
			if pkg.PkgPath == item.Name {
				dstPkg = pkg
				break
			}
		}
		if dstPkg == nil {
			return fmt.Errorf("no such directory %s", item.Name)
		}
		pos, end = snap.FileRange(dstPkg.Files[0].Syntax.Pos())

	case refactor.ItemPos:
		pos, end = item.Pos, item.End
	}

	for _, impPath := range imports {
		var alias, ppath string
		if a, p, ok := cut(impPath, " "); ok {
			alias, ppath = a, strings.TrimLeft(p, " \t")
		} else {
			alias, ppath = "", impPath
		}

		var tpkg *types.Package
		for _, p := range snap.Packages() {
			if p.PkgPath == ppath && p.Types != nil {
				tpkg = p.Types
				break
			}
		}
		if tpkg == nil {
			tpkg = types.NewPackage(ppath, path.Base(ppath))
		}
		snap.NeedImport(pos, alias, tpkg)
	}

	var old string
	if cmd == "sub" {
		old = string(snap.Text(pos, end))
		snap.DeleteAt(pos, end)
	}
	if cmd == "add" || strings.HasSuffix(old, "\n") {
		// TODO: Is final \n a good idea?
		text += "\n"
	}
	snap.InsertAt(end, text)
	return nil
}
