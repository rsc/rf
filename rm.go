// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"bytes"
	"go/ast"
	"go/token"
	"go/types"
	"strings"

	"rsc.io/rf/refactor"
)

func cmdRm(snap *refactor.Snapshot, argsText string) (more []string, exp bool) {
	args := strings.Fields(argsText)
	if len(args) < 1 {
		snap.ErrorAt(token.NoPos, "usage: rm addr...\n")
		return
	}

	rm := make(map[types.Object]bool)
	for _, arg := range args {
		item := snap.Lookup(arg)
		if item == nil {
			snap.ErrorAt(token.NoPos, "cannot find %s", arg)
			continue
		}
		switch item.Kind {
		default:
			snap.ErrorAt(token.NoPos, "rm %s: %v not supported", arg, item.Kind)
			continue
		case refactor.ItemConst, refactor.ItemFunc, refactor.ItemMethod, refactor.ItemType, refactor.ItemVar:
			rm[item.Obj] = true
		}
	}

	removeDecls(snap, rm)

	// TODO: If exported API is being deleted, maybe return exp = true
	// so that importers are re-checked for still compiling.
	return nil, false
}

func removeDecls(snap *refactor.Snapshot, rm map[types.Object]bool) {
	did := make(map[*ast.GenDecl]bool)
Objs:
	for obj := range rm {
		stack := snap.SyntaxAt(obj.Pos())
		pkg, _ := snap.FileAt(obj.Pos())
		switch stack[1].(type) {
		default:
			// TODO *ast.AssignStmt
			snap.ErrorAt(obj.Pos(), "cannot delete declaration of %s", obj.Name())

		case *ast.ValueSpec:
			decl := stack[2].(*ast.GenDecl)
			if did[decl] {
				break
			}
			did[decl] = true
			// determine what parts of decl are being removed.
			// doc comments too

			complete := true
			any := false
			for _, s := range decl.Specs {
				switch s := s.(type) {
				default:
					snap.ErrorAt(obj.Pos(), "cannot delete declaration of %s", obj.Name())
					continue Objs

				case *ast.ValueSpec:
					for _, id := range s.Names {
						if rm[pkg.TypesInfo.Defs[id]] {
							any = true
						} else {
							complete = false
						}
					}
				}
			}

			if complete {
				// Delete entire declaration or block.
				snap.DeleteAt(nodeRange(snap, decl))
				break
			}

			if !any {
				break
			}

			for _, s := range decl.Specs {
				switch s := s.(type) {
				case *ast.ValueSpec:
					complete := true
					any := false
					for _, id := range s.Names {
						if rm[pkg.TypesInfo.Defs[id]] {
							any = true
						} else {
							complete = false
						}
					}
					if complete {
						// Delete entire spec (inside block).
						snap.DeleteAt(nodeRange(snap, s))
						break
					}
					if !any {
						break
					}
					// Delete only some names in spec.
					// Initializer too.
					for i, id := range s.Names {
						if rm[pkg.TypesInfo.Defs[id]] {
							if i == 0 {
								snap.DeleteAt(id.Pos(), s.Names[1].Pos())
								if len(s.Values) > 0 {
									snap.DeleteAt(s.Values[0].Pos(), s.Values[1].Pos())
								}
							} else {
								snap.DeleteAt(s.Names[i-1].End(), id.End())
								if len(s.Values) > 0 {
									snap.DeleteAt(s.Values[i-1].End(), s.Values[i].End())
								}
							}
						}
					}
				}
			}
		}
	}
}

var (
	slashSlash = []byte("//")
	starSlash  = []byte("*/")
)

func nodeRange(snap *refactor.Snapshot, n ast.Node) (pos, end token.Pos) {
	startFile, endFile := snap.FileRange(n.Pos())
	text := snap.Text(startFile, endFile)

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
