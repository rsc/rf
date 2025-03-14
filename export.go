package main

import (
	"go/token"
	"strings"

	"rsc.io/rf/refactor"
)

func cmdExport(snap *refactor.Snapshot, args string) {
	args = strings.TrimSpace(args)
	if args == "" {
		snap.ErrorAt(token.NoPos, "usage: export old...")
		return
	}
	items, _ := snap.EvalList(args)
	for _, item := range items {
		switch item.Kind {
		case refactor.ItemNotFound:
			snap.ErrorAt(token.NoPos, "cannot find %s", item.Name)
			return
		case refactor.ItemFile,
			refactor.ItemPos:
			snap.ErrorAt(token.NoPos, "cannot export %v", item.Kind)
			return
		default:
			// TODO export all items in file or text.
			snap.ErrorAt(token.NoPos, "cannot export %v", item.Kind)
			return
		case refactor.ItemConst,
			refactor.ItemType,
			refactor.ItemVar,
			refactor.ItemFunc,
			refactor.ItemField,
			refactor.ItemMethod:
		}
	}
	for _, old := range items {
		_, oldName, ok := cutLast(old.Name, ".")
		if !ok {
			oldName = old.Name
		}
		newName := exportSym(oldName)
		rewriteDefn(snap, old, newName)
		rewriteUses(snap, old, newName, notInScope(newName))
	}
}

func exportSym(sym string) string {
	return strings.Title(strings.TrimLeft(sym, "_"))
}
