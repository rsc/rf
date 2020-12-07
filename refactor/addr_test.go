// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package refactor

import "testing"

// input text for matches.
// Note that every line is 5 bytes,
// so absolute addresses are easy to calculate.
var testInput = `1 10
2 20
3 30
4 40
5 50
6 60
7 70
8 80
9 90
`

var addrToByteRangeTests = []struct {
	start  int
	addr   string
	lo, hi int
}{
	{0, "0", 0, 0},
	{0, "1", 0, 5},
	{0, "2", 5, 10},
	{0, "/10/", 2, 4},
	{0, "/10/+#0", 4, 4},
	{0, "/10/+0", 5, 5},
	{0, "/10/+", 5, 10},
	{0, "/10/+1", 5, 10},
	{0, "/10/+2", 10, 15},
	{0, "/10/+3", 15, 20},
	{0, "/90/", 42, 44},
	{0, "/90/-#0", 42, 42},
	{0, "/90/-0", 40, 40},
	{0, "/90/-", 35, 40},
	{0, "/90/-1", 35, 40},
	{0, "/90/-2", 30, 35},
	{0, "/10/,/90/", 2, 44},
	{0, "/10/,/90/-#0", 2, 42},
	{0, "/10/,/90/-0", 2, 40},
	{0, "/10/,/90/-", 2, 40},
	{0, "/10/,/90/-1", 2, 40},
	{0, "/10/,/90/-2", 2, 35},
	{0, "/10/,/90/-3", 2, 30},
	{0, "/10/,/90/-4", 2, 25},
	{0, "/10/,/90/-5", 2, 20},
	{0, "/10/,/90/-6", 2, 15},
	{0, "/10/,/90/-7", 2, 10},
	{0, "/10/,/90/-8", 2, 5},

	{0, "/50/", 22, 24},
	{0, "/50/-", 15, 20},
	{0, "/50/--", 10, 15},
	{0, "/50/---", 5, 10},
	{0, "/50/+", 25, 30},
	{0, "/50/++", 30, 35},
	{0, "/50/-0", 20, 20},
	{0, "/50/-0+", 20, 25},
	{0, "/50/-+", 20, 25},
	{0, "/50/-0+0", 20, 20},
	{0, "/50/-+0", 20, 20},
	{0, "/50/-1+0", 20, 20},
	{0, "/10/-0", 0, 0},
	{0, "/10/-0+", 0, 5},
	{0, "/10/-+", 0, 5},
	{0, "/90/+0", 45, 45},
	{0, "/90/+0-", 40, 45},
	{0, "/90/+-", 40, 45},
}

func TestAddrToByteRange(t *testing.T) {
	data := []byte(testInput)
	for _, tt := range addrToByteRangeTests {
		lo, hi, err := addrToByteRange(tt.addr, tt.start, data)
		if lo != tt.lo || hi != tt.hi || err != nil {
			t.Errorf("addrToByteRange(%#q, %d, data) = %d, %d, %v, want %d, %d, nil", tt.addr, tt.start, lo, hi, err, tt.lo, tt.hi)
		}
	}
}
