// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"bytes"
	"errors"
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
	fmt.Fprintf(os.Stderr, "usage: rf [-diff] script\n")
	os.Exit(2)
}

func main() {
	log.SetPrefix("rf: ")
	log.SetFlags(0)

	flag.Usage = usage
	flag.Parse()
	args := flag.Args()
	if len(args) != 1 {
		usage()
	}
	script := args[0]

	rf, err := refactor.New(".")
	if err != nil {
		log.Fatal(err)
	}
	rf.ShowDiff = *showDiff
	if err := run(rf, script); err != nil {
		log.Fatal(err)
	}
}

var cmds = map[string]func(*refactor.Snapshot, string) error{
	"add":        cmdAdd,
	"sub":        cmdSub,
	"debug":      cmdDebug,
	"inline":     cmdInline,
	"key":        cmdKey,
	"ex":         cmdEx,
	"mv":         cmdMv,
	"rm":         cmdRm,
	"typeassert": cmdTypeAssert,
	"inject":     cmdInject,
}

func run(rf *refactor.Refactor, script string) error {
	var snaps []*refactor.Snapshot

	text := script
	lastCmd := ""

	defer func() {
		if e := recover(); e != nil {
			println("panic executing: " + lastCmd)
			panic(e)
		}
	}()

	for text != "" {
		line, rest, err := readLine(text)
		if err != nil {
			return err
		}
		text = rest
		if line == "" {
			continue
		}

		cmd, args, _ := cutAny(line, " \t")

		if rf.Debug["trace"] != "" {
			fmt.Fprintf(os.Stderr, "> %s\n", line)
		}

		fn := cmds[cmd]
		if fn == nil {
			return fmt.Errorf("unknown command %s", cmd)
		}

		snaps, err = rf.Snapshots()
		if err != nil {
			if lastCmd == "" {
				return wrapError(err, "errors found before executing script")
			}
			return err
		}
		x, _, ok := cut(line, "\n")
		if ok {
			x += " \\ ..."
		}
		lastCmd = x

		for _, snap := range snaps {
			targ := snap.Target()
			if targ == nil {
				return fmt.Errorf("missing target")
			}
			if targ.Types == nil {
				return fmt.Errorf("no types in target")
			}

			if err := fn(snap, args); err != nil {
				return err
			}

			if err := snap.Errors.Err(); err != nil {
				return wrapError(err, "errors found during: %s", lastCmd)
			}

			snap.Gofmt()
			if err := snap.Errors.Err(); err != nil {
				return wrapError(err, "errors found during gofmt after: %s", lastCmd)
			}
		}

		switch err := rf.Apply(); err.(type) {
		case nil:
		case *refactor.ErrorList:
			// Show diff so errors are easier to understand.
			snap, mergeErr := rf.MergeSnapshots()
			if mergeErr != nil {
				err = fmt.Errorf("%w\nalso merging snapshots failed:\n%w", err, mergeErr)
			} else if d, err := snap.Diff(); err == nil {
				rf.Stdout.Write(d)
			}
			return wrapError(err, "errors found after executing: %s", lastCmd)
		default:
			return fmt.Errorf("checking rewritten packages: %w", err)
		}
	}

	if snaps == nil {
		// Did nothing.
		return nil
	}

	snap, err := rf.MergeSnapshots()
	if err != nil {
		// Snapshots diverged.
		return err
	}
	if rf.ShowDiff {
		d, err := snap.Diff()
		if err != nil {
			return err
		}
		rf.Stdout.Write(d)
		return nil
	}
	return snap.Write()
}

func wrapError(err error, f string, args ...interface{}) error {
	var l *refactor.ErrorList
	if errors.As(err, &l) {
		// For an error list, print all of l first, followed by the new message,
		// on the assumption that there may be many errors and the new message
		// summarizes them.
		return fmt.Errorf("%w\n%s", err, fmt.Sprintf(f, args...))
	}
	// Follow the normal error message wrapping convention.
	return fmt.Errorf("%s: %w", fmt.Sprintf(f, args...), err)
}

func readLine(text string) (line, rest string, err error) {
	text = strings.TrimLeft(text, " \t\n")
	var (
		keep       bytes.Buffer
		punct      []byte
		sawPunct   bool
		start      int
		quote      byte
		escNL      bool
		regexp     bool
		regexpText string
	)
	for i := 0; i < len(text); i++ {
		switch c := text[i]; {
		case quote == 0 && c == '#':
			// Cut comment to end of line, but leave \n for next iteration.
			keep.WriteString(text[start:i])
			j := strings.IndexByte(text[i:], '\n')
			if j < 0 {
				j = len(text)
			} else {
				j += i
			}
			start = j
			i = start - 1

		case quote != 0 && c == quote:
			quote = 0

		case c == '"', c == '\'', c == '`':
			quote = c

		case !sawPunct && c == ':' && !regexp:
			if i+1 < len(text) && text[i+1] == '/' {
				regexp = true
				i++
				regexpText = text[i:]
			}

		case regexp && c == '/':
			if i+2 < len(text) && (text[i+1] == '+' || text[i+1] == '-' || text[i+1] == ',') && text[i+2] == '/' {
				// regexp stays true
				i += 2
				regexpText = text[i:]
				continue
			}
			regexp = false

		case c == '\\' && (quote == '\'' || quote == '"' || regexp):
			i++

		case c == '\n' && (quote == '\'' || quote == '"'):
			return "", "", fmt.Errorf("newline in %c-quoted string", quote)

		case c == '\n' && regexp:
			return "", "", fmt.Errorf("newline in regexp search: %s", regexpText[:strings.Index(regexpText, "\n")])

		case quote != 0 || regexp:
			continue

		case c == '\\':
			j := i + 1
			for j < len(text) && text[j] == ' ' || text[j] == '\t' {
				j++
			}
			if text[j] == '\n' || text[j] == '#' {
				escNL = true // do not break at next \n
				keep.WriteString(text[start:i])
				start = j
				i = j - 1
				continue
			}

		case c == '\n' && len(punct) == 0:
			if escNL {
				escNL = false
				continue
			}
			keep.WriteString(text[start:i])
			rest = text[i+1:]
			return keep.String(), rest, nil

		case c == '{', c == '[', c == '(':
			sawPunct = true
			punct = append(punct, c)

		case c == '}', c == ']', c == ')':
			var open byte
			switch c {
			case '}':
				open = '{'
			case ']':
				open = '['
			case ')':
				open = '('
			}
			if len(punct) == 0 || punct[len(punct)-1] != open {
				return "", "", fmt.Errorf("unexpected %c", c)
			}
			punct = punct[:len(punct)-1]
		}
	}
	if quote != 0 {
		return "", "", fmt.Errorf("unterminated %c-quoted string", quote)
	}
	if len(punct) > 0 {
		return "", "", fmt.Errorf("unclosed %c", punct[len(punct)-1])
	}
	keep.WriteString(text[start:])
	return keep.String(), "", nil
}

func trimComments(line string) string {
	// Cut line at # comment, being careful not to cut inside quoted text.
	var q byte
	for i := 0; i < len(line); i++ {
		switch c := line[i]; c {
		case q:
			q = 0
		case '\'', '"', '`':
			q = c
		case '\\':
			if q == '\'' || q == '"' {
				i++
			}
		case '#':
			if q == 0 {
				line = line[:i]
			}
		}
	}
	return strings.TrimSpace(line)
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

func cmdDebug(snap *refactor.Snapshot, text string) error {
	for _, f := range strings.Fields(text) {
		key, val, ok := cut(f, "=")
		if !ok {
			val = "1"
		}
		snap.Refactor().Debug[key] = val
	}
	return nil
}
