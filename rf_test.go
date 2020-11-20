// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"bytes"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"testing"

	"golang.org/x/tools/txtar"
	"rsc.io/rf/diff"
	"rsc.io/rf/refactor"
)

var updateTestData = flag.Bool("u", false, "update testdata instead of failing")

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
			t.Log(file)
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

			var stdout, stderr bytes.Buffer
			rf, err := refactor.New(dir)
			if err != nil {
				t.Fatal(err)
			}
			rf.Stdout = &stdout
			rf.Stderr = &stderr
			rf.ShowDiff = true
			if err := run(rf, string(ar.Comment)); err != nil {
				fmt.Fprintf(rf.Stderr, "ERROR: %v\n", err)
			}

			if *updateTestData {
				stderrChanged := updateFile(ar, "stderr", stderr.Bytes())
				stdoutChanged := updateFile(ar, "stdout", stdout.Bytes())
				if stdoutChanged || stderrChanged {
					if err := ioutil.WriteFile(file, txtar.Format(ar), 0666); err != nil {
						t.Fatal(err)
					}
					t.Log("updated")
				}
				return
			}

			cmp := func(name string, have, want []byte) {
				have = trimSpace(have)
				want = trimSpace(want)
				if !bytes.Equal(have, want) {
					t.Errorf("%s:\n%s", name, have)
					t.Errorf("want:\n%s", want)
					d, err := diff.Diff("want", want, "have", have)
					if err == nil {
						t.Errorf("diff of diffs:\n%s", d)
					}
				}
			}
			cmp("stderr", stderr.Bytes(), wantStderr.Data)
			cmp("stdout", stdout.Bytes(), wantStdout.Data)
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

func updateFile(ar *txtar.Archive, name string, data []byte) bool {
	data = trimSpace(data)

	for i := range ar.Files {
		if file := &ar.Files[i]; file.Name == name {
			if len(data) == 0 {
				ar.Files = append(ar.Files[:i], ar.Files[i+1:]...)
				return true
			}

			if !bytes.Equal(file.Data, data) {
				file.Data = data
				return true
			}

			return false
		}
	}

	if len(data) != 0 {
		ar.Files = append(ar.Files, txtar.File{
			Name: name,
			Data: data,
		})
		return true
	}

	return false
}
