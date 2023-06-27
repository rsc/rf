// Copyright 2023 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import "fmt"

// errUsage indicates a syntax error in a refactoring command. Usage errors are
// independent of the source code being refactored.
type errUsage struct {
	err string
}

func newErrUsage(f string, args ...interface{}) *errUsage {
	return &errUsage{fmt.Sprintf(f, args...)}
}

func (e *errUsage) Error() string {
	return "usage: " + e.err
}

// errPrecondition indicates that a refactoring command was well-formed, but
// some requirement of the command wasn't met by the current Snapshot. For
// example, a referenced identifier wasn't found.
type errPrecondition struct {
	err string
}

func newErrPrecondition(f string, args ...interface{}) *errPrecondition {
	return &errPrecondition{fmt.Sprintf(f, args...)}
}

func (e *errPrecondition) Error() string {
	return e.err
}
