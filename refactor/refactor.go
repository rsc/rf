// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package refactor

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
)

// A Refactor holds the state for an active refactoring.
type Refactor struct {
	Stdout   io.Writer
	Stderr   io.Writer
	ShowDiff bool

	dir     string
	modRoot string
	modPath string
}

// New returns a new refactoring,
// editing the package in the given directory (usually ".").
func New(dir string) (*Refactor, error) {
	if info, err := os.Stat(dir); err != nil {
		return nil, err
	} else if !info.IsDir() {
		return nil, fmt.Errorf("not a directory: %s", dir)
	}

	d, err := filepath.Abs(dir)
	if err != nil {
		return nil, err
	}
	dir = d

	r := &Refactor{
		Stdout: os.Stdout,
		Stderr: os.Stderr,
		dir:    dir,
	}
	return r, nil
}

// shortPath returns an absolute or relative name for path, whatever is shorter.
func (r *Refactor) shortPath(path string) string {
	if rel, err := filepath.Rel(r.dir, path); err == nil && len(rel) < len(path) {
		return rel
	}
	return path
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
