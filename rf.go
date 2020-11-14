// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

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
	if err := run(rf, script); err != nil {
		log.Fatal(err)
	}
}

var cmds = map[string]func(*refactor.Snapshot, string) ([]string, bool){
	"mv": cmdMv,
	"ex": cmdEx,
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
		cmd, args, _ := cutAny(line, " \t")
		if cmd == "" {
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

		if len(more) > 0 || exp {
			if exp {
				pkgs, err := rf.Importers(snap)
				if err != nil {
					return err
				}
				more = append(more, pkgs...)
			}
			snap, err = base.Load(more...)
			if err != nil {
				return err
			}

			evenMore, _ := fn(snap, args)
			if snap.Errors() > 0 {
				return err
			}
			if len(evenMore) > 0 {
				return fmt.Errorf("%s did not converge: after %v, needs %v", cmd, more, evenMore)
			}
		}

		snap.Gofmt()
		base = snap
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

	return snap.Write()
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
