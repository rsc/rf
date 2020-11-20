// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Rf refactors Go programs.
//
// Usage:
//
//	rf [-diff] script
//
// Rf applies a script of refactoring commands to the package in the current directory.
// For example, to unexport a field in a struct by renaming it:
//
//	rf 'mv T.Field T.field'
//
// By default, rf writes changes back to the disk.
// The -diff flag causes rf to print a diff of the intended changes instead.
//
// A script is a sequence of commands, one per line.
// Comments are introduced by # and extend to the end of the line.
// Commands may be broken across lines by ending all but the last
// with a trailing backslash (before any comment), as in:
//
//	rf '
//		# command
//		mv T.Field \ # source
//		   T.field   # destination
//	'
//
// Code addresses
//
// Most commands take “code addresses” as arguments.
// Each code address identifies some code in a program.
// For illustration, consider this program, prog.go:
//
//	package p
//
//	const (
//		C = iota
//		D
//	)
//
//	func F() {
//		who := "world"
//		msg := fmt.Sprintf("hello, %v", who)
//		fmt.Printf("%s\n", msg)
//	}
//
//	type T struct { Field int }
//
//	func (t T) M() string {
//		return fmt.Sprint(t)
//	}
//
//	func (*T) P() {}
//
//	var V struct {
//		Value int
//		thing T
//	}
//
//	var VT T
//
// The simplest code address is the name of a top-level declaration.
// In this program, those addresses are C, D, F, T, V, and VT.
//
// Adding .Name to an address selects a name within the earlier address,
// whether that's a function variable (F.who, F.msg),
// a struct field (T.Field), a method (T.M, T.P),
// or a variable's field (V.Value, V.thing, V.thing.Field, VT.Field).
//
// Another kind of code address is a Go source file, identified by
// a name ending in “.go”, as in “file.go”. If the file name contains
// a slash, as in “../dir/file.go”, the address identifies a file in
// the package in “../dir”.
//
// Another kind of code address is a Go package, identified by a path
// containing a slash but not ending in “.go”, as in “../dir” or
// “example.com/pkg”.
//
// A final kind of code address is a textual range in a function body
// or source file, identified by the syntax Ident:Range, where Ident
// identifies a file, function, or method and Range identifies a
// section of text within. The Range syntax is as used in the
// Acme and Sam text editors. The most common forms are the line range
// “N,M”, the byte range “#N,M”, and the regular expression range
// “/re1/,/re2/”. For example:
//
//	prog.go:9                 # "world" line
//	prog.go:9,10              # "world" and fmt.Sprintf lines
//  prog.go:/msg/-0,/fmt/+0   # "world" and fmt.Sprintf lines
//  F:/msg/-0,/fmt/+0         # (same)
//  prog.go:/fmt.*/           # the fmt.Sprintf call in F
//  F:/fmt.Sprint.*/          # (same)
//  T.M:/fmt.*/               # the fmt.Sprint call in T.M
//
// The add command
//
// The add command adds to the package. It takes as an argument the
// address where the text should be added, followed by any text to include.
//
//	add address text...
//
// If the address is a non-existent struct field in an existing struct (T.NewField),
// the text gives the type and optional struct tag of the new field,
// which will be added at the end of the struct.
//
// If the address is a file, the text will be added at the end of the file.
// (UNIMPLEMENTED)
//
// Examples:
//
//	add T.NewField int
//	add x.go func F() {}
//
// The key command
//
// The key command converts all struct literals for a list of types to keyed literals.
//
//	key address...
//
// Each address must identify a struct type. All literals of those struct types are
// updated to use the keyed form.
//
// Example:
//
//	key Point
//
// The cp command
//
// TODO cp is like mv but doesn't delete the source and doesn't update any references.
//
// The ex command
//
// The ex command applies rewrites based on example snippets.
//
//	ex [imports] [declarations] old->new...
//
// The arguments to ex are interpreted as Go code consisting of a
// sequence of imports and then a list of special “old -> new” rules.
// Each rule specifies that where ex finds a pattern matching old,
// it should replace the code with new. For example, to replace all
// log.Error calls with log.Panic:
//
//	ex import "log"; log.Error -> log.Panic
//
// Declarations introduce typed pattern variables that can be used
// in rules. For example, to simplify certain needlessly complex
// uses of fmt.Sprintf:
//
//	ex \
//		import "fmt"; \
//		import "strconv"; \
//		var s string; \
//		fmt.Sprintf("%s", s) -> s; \
//		fmt.Sprintf("%v", s) -> s; \
//		fmt.Sprintf("%q", s) -> strconv.Quote(s)
//
// The semicolons are required, because the newlines are elided
// when the lines are unwrapped.
//
// The mv command
//
// The mv command moves and renames code.
//
//	mv [-f] source... destination
//
// When mv moves or renames code, it also updates any references
// to use the new names or locations for the code.
// This includes updating other packages in the current module.
//
// In general, mv aims to act appropriately for any sensible
// combination of source kind and destination kind.
// The rest of this section enumerates the specific cases that
// mv handles.
//
// item → renamed item
//
// Any named item can be renamed by specifying a destination
// that is the same code address with the final element changed.
// For example:
//
//	mv Point.x Point.X  # struct field

// If the source is a struct field, the destination must be a field
// in the same struct, and mv renames the field. For example:
//
//	mv Point.x Point.X
//	mv Point.y Point.Y
//
// If the source is a top-level const, func, type, or var, and the
// destination names a non-existent top-level name,
// then mv
//
// var → var field
//
// TODO
//
// method → func
//
// TODO
//
// func → method
//
// TODO
//
// code text → new function
//
// TODO
//
// code text → new method
//
// TODO
//
// declaration → file
//
// If the source is a top-level declaration (const, func, method, type, var)
// and the destination is a file, mv moves that declaration, along with
// any comments immediately preceding it, to the end of the
// destination file. For example:
//
//	mv Template NewTemplate Template.Method thing.go
//
// Naming a single item in a declaration block is taken to indicate
// wanting to move the entire block.
//
// Any time a destination file must be created, mv initializes it
// with the header comments (those above the package declaration
// and any package doc) from the file the source code is being
// moved from. This heuristic is meant to copy header text like copyright notices.
//
// file → file
//
// TODO
//
// file → package
//
// TODO
//
// package → file
//
// TODO
//
// package → package
//
// TODO
//
// The rm command
//
// TODO
//
// Bugs Bugs Bugs
//
// Rf is very very rough. Everything is subject to change, and it may break your programs.
//
package main

import (
	"flag"
	"fmt"
	"log"
	"os"
	"regexp"
	"strings"
	"unicode/utf8"

	"rsc.io/rf/refactor"
)

var showDiff = flag.Bool("diff", false, "show diff instead of writing files")

func usage() {
	fmt.Fprintf(os.Stderr, "usage: rf [-diff] script [pkg ...]\n")
	os.Exit(2)
}

func main() {
	log.SetPrefix("rf: ")
	log.SetFlags(0)

	flag.Usage = usage
	flag.Parse()
	args := flag.Args()
	if len(args) < 1 {
		usage()
	}
	script, pkgs := args[0], args[1:]

	rf, err := refactor.New(".", pkgs...)
	if err != nil {
		log.Fatal(err)
	}
	rf.ShowDiff = *showDiff
	if err := run(rf, script); err != nil {
		log.Fatal(err)
	}
}

var cmds = map[string]func(*refactor.Snapshot, string) ([]string, bool){
	"add":    cmdAdd,
	"inline": cmdInline,
	"key":    cmdKey,
	"ex":     cmdEx,
	"mv":     cmdMv,
	"rm":     cmdRm,
}

type loader interface {
	Load(...string) (*refactor.Snapshot, error)
}

func run(rf *refactor.Refactor, script string) error {
	var base loader = rf
	var snap *refactor.Snapshot

	text := script
	for text != "" {
		var line string
		line, text, _ = cut(text, "\n")
		line = strings.TrimSpace(line)
		for strings.HasSuffix(line, `\`) && text != "" {
			var l string
			l, text, _ = cut(text, "\n")
			line = line[:len(line)-1] + " " + l
			line = strings.TrimSpace(line)
		}
		cmd, args, _ := cutAny(line, " \t")
		if cmd == "" || cmd[0] == '#' {
			continue
		}

		fn := cmds[cmd]
		if fn == nil {
			return fmt.Errorf("unknown command %s", cmd)
		}

		var err error
		snap, err = base.Load()
		if err != nil {
			return err
		}

		more, exp := fn(snap, args)
		if snap.Errors() > 0 {
			return err
		}
		if len(more) > 0 {
			snap, err = base.Load(more...)
			if err != nil {
				return err
			}

			var evenMore []string
			evenMore, exp = fn(snap, args)
			if snap.Errors() > 0 {
				return err
			}
			if len(evenMore) > 0 {
				return fmt.Errorf("%s did not converge: after %v, needs %v", cmd, more, evenMore)
			}
		}
		if exp {
			pkgs, err := rf.Importers(snap)
			if err != nil {
				return err
			}
			more = append(more, pkgs...)
			snap, err = base.Load(more...)
			if err != nil {
				return err
			}

			fn(snap, args)
			if snap.Errors() > 0 {
				return err
			}
		}

		snap.Gofmt()
		base = snap
		snap.Write() // TODO: Should not be necessary.
	}

	if snap == nil {
		// Did nothing.
		return nil
	}

	// Show diff before final load, so that it's easier to understand errors.
	if rf.ShowDiff {
		d, err := snap.Diff()
		if err != nil {
			return err
		}
		rf.Stdout.Write(d)
	}

	// Reload packages one last time before writing,
	// to make sure the rewrites are valid.
	if _, err := snap.Load(snap.Modified()...); err != nil {
		return fmt.Errorf("checking rewritten packages: %v", err)
	}

	if rf.ShowDiff {
		return nil
	}

	return nil // TODO snap.Write()
}

var isGoIdent = regexp.MustCompile(`^[\p{L}_][\p{L}\p{Nd}_]*$`)

func topItem(item *refactor.Item) *refactor.Item {
	for item != nil && item.Outer != nil {
		item = item.Outer
	}
	return item
}

func cut(s, sep string) (before, after string, ok bool) {
	if i := strings.Index(s, sep); i >= 0 {
		return s[:i], s[i+len(sep):], true
	}
	return s, "", false
}

func cutAny(s, any string) (before, after string, ok bool) {
	if i := strings.IndexAny(s, any); i >= 0 {
		_, size := utf8.DecodeRuneInString(s[i:])
		return s[:i], s[i+size:], true
	}
	return s, "", false
}

func cutLast(s, sep string) (before, after string, ok bool) {
	if i := strings.LastIndex(s, sep); i >= 0 {
		return s[:i], s[i+len(sep):], true
	}
	return s, "", false
}
