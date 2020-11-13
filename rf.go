// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"regexp"
	"strings"

	"rsc.io/rf/refactor"
)

var showDiff = flag.Bool("diff", false, "show diff instead of writing files")

func usage() {
	fmt.Fprintf(os.Stderr, "usage: rf [-diff] 'old->new'...\n")
	os.Exit(2)
}

func main() {
	log.SetPrefix("rf: ")
	log.SetFlags(0)

	flag.Usage = usage
	flag.Parse()
	args := flag.Args()
	if len(args) == 0 {
		usage()
	}

	if err := run(".", ".", *showDiff, os.Stdout, os.Stderr, args); err != nil {
		log.Fatal(err)
	}
}

func run(dir, pkg string, showDiff bool, stdout, stderr io.Writer, args []string) error {
	cmd := cmds[args[0]]
	if cmd == nil {
		return fmt.Errorf("unknown command %s", args[0])
	}

	rf, err := refactor.New(dir, pkg)
	if err != nil {
		return err
	}
	rf.Stdout = stdout
	rf.Stderr = stderr
	rf.ShowDiff = showDiff

	snap, err := rf.LoadTyped()
	if err != nil {
		return fmt.Errorf("loading initial package: %v", err)
	}

	morePackages, needImporters, err := cmd(snap, args[1:])
	if err != nil {
		return err
	}
	if snap.NumErrors() > 0 {
		return fmt.Errorf("errors converting initial package")
	}

	// Might have identified that the transformation
	// was incomplete and needs to be done over again
	// with additional packages.
	if len(morePackages) > 0 || needImporters {
		extra := morePackages
		if needImporters {
			importers, err := rf.Importers()
			if err != nil {
				return err
			}
			extra = append(extra, importers...)
		}
		snap, err = rf.LoadTyped(extra...)
		if err != nil {
			return fmt.Errorf("loading additional packages: %v", err)
		}

		// Rerun the command on the expanded package set.
		evenMore, _, err := cmd(snap, args[1:])
		if err != nil {
			return err
		}
		if len(evenMore) > 0 {
			return fmt.Errorf("after loading %v, need %v - did not converge", morePackages, evenMore)
		}
		if snap.NumErrors() > 0 {
			return fmt.Errorf("errors converting additional packages")
		}
	}

	snap.Gofmt()

	// Show diff before type-checking, so that it's easier to understand errors.
	if rf.ShowDiff {
		d, err := snap.Diff()
		if err != nil {
			return err
		}
		stdout.Write(d)
	}

	// Reload packages before writing, to make sure the rewrites are valid.
	if _, err := rf.LoadTyped(snap.Modified()...); err != nil {
		return fmt.Errorf("checking rewritten packages: %v", err)
	}

	if rf.ShowDiff {
		return nil
	}

	return snap.Write(stderr)
}

var cmds = map[string]func(*refactor.Snapshot, []string) (morePkgs []string, changesExports bool, err error){
	"mv": cmdMv,
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

func cutLast(s, sep string) (before, after string, ok bool) {
	if i := strings.LastIndex(s, sep); i >= 0 {
		return s[:i], s[i+len(sep):], true
	}
	return s, "", false
}
