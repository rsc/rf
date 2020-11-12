// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package diff

import "testing"

const (
	oldName = "a/b/c"
	newName = "d/e/f"
	oldText = "abc\ndef\nghi\n"
	newText = "ABC\ndef\nGHI\n"
	want    = "diff a/b/c d/e/f\n--- a/b/c\n+++ d/e/f\n@@ -1,3 +1,3 @@\n-abc\n+ABC\n def\n-ghi\n+GHI\n"
)

func TestDiff(t *testing.T) {
	out, err := Diff(oldName, []byte(oldText), newName, []byte(newText))
	if err != nil {
		t.Fatal(err)
	}
	if string(out) != want {
		t.Errorf("Diff: have:\n%s", out)
		t.Errorf("Diff: want:\n%s", want)
	}
}
