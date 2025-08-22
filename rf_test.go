// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"

	"golang.org/x/tools/txtar"
	"rsc.io/rf/diff"
	"rsc.io/rf/refactor"
)

var readLineTests = []struct {
	in  string
	out []string
	err error
}{
	{
		in:  "cmd x y",
		out: []string{"cmd x y"},
	},
	{
		in:  "cmd x \\\ny",
		out: []string{"cmd x \ny"},
	},
	{
		in:  "cmd x \\ # hello\ny",
		out: []string{"cmd x \ny"},
	},
	{
		in:  "cmd x y\n",
		out: []string{"cmd x y"},
	},
	{
		in:  "cmd (\nx y\n)\n",
		out: []string{"cmd (\nx y\n)"},
	},
	{
		in:  "cmd {\nx y\n}\n",
		out: []string{"cmd {\nx y\n}"},
	},
	{
		in:  "cmd It\\'s not a failure\n",
		out: []string{"cmd It's not a failure"},
	},
	{
		in:  "cmd 'It\\'s not a failure'\n",
		out: []string{"cmd 'It\\'s not a failure'"},
	},
	{
		in:  "cmd It\\\"s not a failure\n",
		out: []string{"cmd It\"s not a failure"},
	},
	{
		in:  "cmd \"It\\\"s not a failure\"\n",
		out: []string{"cmd \"It\\\"s not a failure\""},
	},
	{
		in:  "cmd It's a failure\n",
		err: fmt.Errorf("newline in '-quoted string"),
	},
	{
		in:  "cmd It\"s a failure\n",
		err: fmt.Errorf("newline in \"-quoted string"),
	},
}

func TestReadLine(t *testing.T) {
	for _, tt := range readLineTests {
		var out []string
		var err error
		text := tt.in
		for text != "" && err == nil {
			var line string
			line, text, err = readLine(text)
			if line != "" {
				out = append(out, line)
			}
		}
		if !reflect.DeepEqual(out, tt.out) || fmt.Sprint(err) != fmt.Sprint(tt.err) {
			t.Errorf("input:\n%s\nreadLine => %q, %v, want %q, %v",
				tt.in, out, err, tt.out, tt.err)
		}
	}
}

var updateTestData = flag.Bool("u", false, "update testdata instead of failing")
var flagKeep = flag.Bool("keep", false, "keep temporary work directories")

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
			var dir string
			if *flagKeep {
				name := filepath.Base(file)
				name = strings.TrimSuffix(name, filepath.Ext(name))
				dir, err = os.MkdirTemp("", "rf-"+name)
				if err != nil {
					t.Fatal("creating work directory:", err)
				}
				t.Log("dir:", dir)
			} else {
				dir = t.TempDir()
			}
			if err := os.WriteFile(filepath.Join(dir, "go.mod"), []byte("module m\n"), 0666); err != nil {
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
				if err := os.WriteFile(targ, file.Data, 0666); err != nil {
					t.Fatal(err)
				}
			}

			// Process flags in the comment.
			var flags flags
			flagSet := flag.NewFlagSet(filepath.Base(file), flag.ContinueOnError)
			flags.register(flagSet)
			lines := bytes.Split(ar.Comment, []byte("\n"))
			var script strings.Builder
			for _, line := range lines {
				if len(line) > 0 && line[0] == '-' {
					// Flags
					if err := flagSet.Parse(strings.Fields(string(line))); err != nil {
						t.Fatal(err)
					}
				} else {
					script.Write(line)
					script.WriteByte('\n')
				}
			}

			var stdout, stderr bytes.Buffer
			defer func() {
				// Flush stderr to the test log on panic.
				if stderr.Len() == 0 {
					return
				}
				if err := recover(); err != nil {
					s := stderr.String()
					for len(s) > 0 && s != "\n" {
						var line string
						line, s, _ = strings.Cut(s, "\n")
						t.Logf("stderr: %s", line)
					}
					panic(err)
				}
			}()
			rf, err := refactor.New(dir)
			if err != nil {
				t.Fatal(err)
			}
			rf.Stdout = &stdout
			rf.Stderr = &stderr
			flags.apply(rf)
			rf.ShowDiff = true
			if err := run(rf, script.String()); err != nil {
				fmt.Fprintf(rf.Stderr, "%v\n", err)
			}

			if *updateTestData {
				stderrChanged := updateFile(ar, "stderr", stderr.Bytes())
				stdoutChanged := updateFile(ar, "stdout", stdout.Bytes())
				if stdoutChanged || stderrChanged {
					if err := os.WriteFile(file, txtar.Format(ar), 0666); err != nil {
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
