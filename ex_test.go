// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"reflect"
	"testing"
)

var commonRangesTests = []struct {
	x   string
	y   string
	out []rangePair
}{
	{"", "", nil},
	{"x", "x", []rangePair{{0, 0, 1}}},
	{"x", "xy", []rangePair{{0, 0, 1}}},
	{"xy", "x", []rangePair{{0, 0, 1}}},
	{"x", "yx", []rangePair{{0, 1, 1}}},
	{"yx", "x", []rangePair{{1, 0, 1}}},
	{"zx", "zx", []rangePair{{0, 0, 2}}},
	{"zx", "zxy", []rangePair{{0, 0, 2}}},
	{"zxy", "zx", []rangePair{{0, 0, 2}}},
	{"zx", "zyx", []rangePair{{0, 0, 1}, {1, 2, 1}}},
	{"zyx", "zx", []rangePair{{0, 0, 1}, {2, 1, 1}}},
	{"zx", "wx", []rangePair{{1, 1, 1}}},
	{"zx", "wxy", []rangePair{{1, 1, 1}}},
	{"zxy", "wx", []rangePair{{1, 1, 1}}},
	{"zx", "wyx", []rangePair{{1, 2, 1}}},
	{"zyx", "wx", []rangePair{{2, 1, 1}}},
	{"a", "b", nil},
}

func TestCommonRanges(t *testing.T) {
	for _, tt := range commonRangesTests {
		list := commonRanges(tt.x, tt.y)
		if !reflect.DeepEqual(list, tt.out) {
			t.Errorf("commonRanges(%q, %q) = %v, want %v", tt.x, tt.y, list, tt.out)
		}
	}
}
