// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/token"
	"go/types"

	"rsc.io/rf/refactor"
)

func cmdRm(snap *refactor.Snapshot, args string) error {
	items, _ := snap.EvalList(args)
	if len(items) == 0 {
		return newErrUsage("rm old...")
	}
	rm := make(map[types.Object]bool)
	for _, item := range items {
		if item == nil {
			continue
		}
		switch item.Kind {
		default:
			return fmt.Errorf("rm %s: %v not supported", item.Name, item.Kind)
		case refactor.ItemNotFound:
			return newErrPrecondition("%s not found", item.Name)
		case refactor.ItemPos:
			snap.DeleteAt(item.Pos, item.End)
		case refactor.ItemConst, refactor.ItemField, refactor.ItemFunc, refactor.ItemMethod, refactor.ItemType, refactor.ItemVar:
			rm[item.Obj] = true
		}
	}
	removeDecls(snap, rm)
	return nil
}

func removeDecls(snap *refactor.Snapshot, rm map[types.Object]bool) {
	did := make(map[*ast.GenDecl]bool)
Objs:
	for obj := range rm {
		stack := snap.SyntaxAt(obj.Pos())
		pkg, _ := snap.FileAt(obj.Pos())
		switch n := stack[1].(type) {
		default:
			// TODO *ast.AssignStmt
			snap.ErrorAt(obj.Pos(), "cannot delete declaration of %s (unexpected %T)", obj.Name(), n)

		case *ast.Field:
			if len(n.Names) > 1 {
				find := func(needle types.Object, haystack []*ast.Ident) int {
					for idx, ident := range haystack {
						if needle.Pos() == ident.Pos() {
							return idx
						}
					}
					return -1
				}
				switch idx := find(obj, n.Names); {
				case idx < 0:
					snap.ErrorAt(obj.Pos(), "cannot delete unknown field %q", obj.Name())
				case idx == 0:
					start := n.Names[idx].Pos()
					end := n.Names[idx+1].Pos() - 1 // delete trailing comma
					snap.DeleteAt(start, end)
				default:
					start := n.Names[idx-1].End() // delete preceeding space and comma
					end := n.Names[idx].End()
					snap.DeleteAt(start, end)
				}
				break
			}
			if fl, ok := stack[2].(*ast.FieldList); ok && len(fl.List) > 1 {
				find := func(needle *ast.Field, haystack *ast.FieldList) int {
					for idx, field := range haystack.List {
						if needle == field {
							return idx
						}
					}
					return -1
				}
				switch idx := find(n, fl); {
				case idx < 0:
					snap.ErrorAt(obj.Pos(), "cannot delete unknown field %q", obj.Name())
				case idx == 0:
					start, end := nodeRange(snap, n)
					end += 1 // delete trailing comma
					snap.DeleteAt(start, end)
				default:
					start := fl.List[idx-1].End()
					end := fl.List[idx].End()
					snap.DeleteAt(start, end)
				}
				break
			}
			snap.DeleteAt(nodeRange(snap, n))

		case *ast.Ident:
			decl, ok := stack[2].(*ast.FuncDecl)
			if !ok {
				panic(fmt.Sprintf("unexpected node %T", stack[2]))
			}
			snap.DeleteAt(nodeRange(snap, decl))

		case *ast.TypeSpec:
			decl := stack[2].(*ast.GenDecl)
			snap.DeleteAt(nodeRange(snap, decl))

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
					snap.ErrorAt(obj.Pos(), "cannot delete declaration of %s (unexpected %T)", obj.Name(), s)
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
