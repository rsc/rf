// Copyright 2023 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package refactor

import (
	"fmt"
	"go/scanner"
	"go/token"
	"go/types"
	"io"
	"sort"
	"strings"
)

// An Error is an error at a particular source position. It may have attached
// errors at other positions (but those must not have secondary errors).
type Error struct {
	Pos token.Position
	Msg string

	Secondary []*Error
}

func (e *Error) Error() string {
	if e.Pos.IsValid() {
		return fmt.Sprintf("%s: %s", e.Pos, e.Msg)
	} else {
		return e.Msg
	}
}

type errorKey struct {
	pos token.Position
	msg string
}

// ErrorList is a set of Errors. It is also an error itself. The zero value is
// an empty list, ready to use.
type ErrorList struct {
	errs []*Error
	set  map[errorKey]bool
}

// Add adds and error to l. If the error is an Error, scanner.Error, or
// types.Error, it uses the position information from the error. If the error is
// an ErrorList or a scanner.ErrorList, it merges all errors from that list into
// this list. Otherwise, it adds the error with no position information. It
// suppresses duplicate errors (same position and message).
func (l *ErrorList) Add(err error) {
	var e *Error

	switch err := err.(type) {
	case nil:
		return

	case *ErrorList:
		for _, e := range err.errs {
			l.Add(e)
		}
		return

	case scanner.ErrorList:
		for _, e := range err {
			l.Add(e)
		}
		return

	case *Error:
		e = err

	case *scanner.Error:
		e = &Error{err.Pos, err.Msg, nil}

	case types.Error:
		e = &Error{err.Fset.Position(err.Pos), err.Msg, nil}
		if len(l.errs) > 0 && strings.HasPrefix(err.Msg, "\t") {
			// This is a secondary error. Attach it to the primary error.
			last := l.errs[len(l.errs)-1]
			last.Secondary = append(last.Secondary, e)
			return
		}

	default:
		e = &Error{token.Position{}, err.Error(), nil}
	}

	k := errorKey{e.Pos, e.Msg}
	if !l.set[k] {
		if l.set == nil {
			l.set = make(map[errorKey]bool)
		}
		l.errs = append(l.errs, e)
		l.set[k] = true
	}
}

// Error sorts, deduplicates, and returns a "\n" separated list of formatted
// errors. Note that the result does not end in "\n" because the caller is
// expected to add that.
func (l *ErrorList) Error() string {
	if len(l.errs) == 0 {
		return "no errors"
	}

	// Sort the error list.
	sort.Slice(l.errs, func(i, j int) bool {
		p1, p2 := l.errs[i].Pos, l.errs[j].Pos
		if p1.Filename != p2.Filename {
			return p1.Filename < p2.Filename
		}
		return p1.Offset < p2.Offset
	})

	// Collapse duplicate messages that appear in many locations on the
	// assumption that the refactoring amplified some issue and the user doesn't
	// want to be flooded.
	count := make(map[string]int)
	for _, e := range l.errs {
		count[e.Msg]++
	}

	// Print messages.
	buf := new(strings.Builder)
	for _, e := range l.errs {
		msg := e.Msg
		switch {
		case count[msg] > 3:
			n := count[e.Msg]
			count[e.Msg] = -1
			msg += fmt.Sprintf(" [× %d]", n)

		case count[msg] < 0:
			continue
		}

		if buf.Len() > 0 {
			buf.WriteByte('\n')
		}

		if e.Pos.IsValid() {
			fmt.Fprintf(buf, "%s: %s", e.Pos, msg)
		} else {
			fmt.Fprintf(buf, "%s", msg)
		}
		for _, e2 := range e.Secondary {
			fmt.Fprintf(buf, "\n%s", e2)
		}
	}
	return buf.String()
}

// Err returns an error equivalent to this error list.
// If the list is empty, Err returns nil.
func (l *ErrorList) Err() error {
	if len(l.errs) == 0 {
		return nil
	}
	return l
}

func (l *ErrorList) flushOnPanic(w io.Writer) {
	if len(l.errs) == 0 {
		// If there are no errors to flush, don't even affect the panic chain.
		return
	}

	p := recover()
	if p == nil {
		return
	}
	defer panic(p)

	fmt.Fprintf(w, "%s\n", l.Error())
}
