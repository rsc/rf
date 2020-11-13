// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"golang.org/x/tools/txtar"
)

func TestRun(t *testing.T) {
	files, err := filepath.Glob("testdata/*.txt")
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatal("no test cases")
	}

	for _, file := range files {
		t.Run(filepath.Base(file), func(t *testing.T) {
			ar, err := txtar.ParseFile(file)
			if err != nil {
				t.Fatal(err)
			}
			dir := t.TempDir()
			if err := ioutil.WriteFile(filepath.Join(dir, "go.mod"), []byte("module m\n"), 0666); err != nil {
				t.Fatal(err)
			}
			var wantStdout, wantStderr txtar.File
			for _, file := range ar.Files {
				if file.Name == "stdout" {
					wantStdout = file
					continue
				}
				if file.Name == "stderr" {
					wantStderr = file
					continue
				}
				targ := filepath.Join(dir, file.Name)
				if err := os.MkdirAll(filepath.Dir(targ), 0777); err != nil {
					t.Fatal(err)
				}
				if err := ioutil.WriteFile(targ, file.Data, 0666); err != nil {
					t.Fatal(err)
				}
			}

			args := strings.Fields(string(ar.Comment))
			var stdout, stderr bytes.Buffer
			err = run(dir, ".", true, &stdout, &stderr, args)
			if err != nil {
				fmt.Fprintf(&stderr, "ERROR: %v\n", err)
			}
			cmp := func(name string, have, want []byte) {
				have = trimSpace(have)
				want = trimSpace(want)
				if !bytes.Equal(have, want) {
					t.Errorf("%s:\n%s", name, have)
					t.Errorf("want:\n%s", want)
				}
			}
			cmp("stdout", stdout.Bytes(), wantStdout.Data)
			cmp("stderr", stderr.Bytes(), wantStderr.Data)
		})
	}
}

func trimSpace(data []byte) []byte {
	lines := bytes.Split(data, []byte("\n"))
	for i, line := range lines {
		lines[i] = bytes.TrimRight(line, " ")
	}
	return bytes.Join(lines, []byte("\n"))
}
