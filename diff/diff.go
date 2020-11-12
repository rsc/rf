// Copyright 2019 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package diff implements a Diff function that compare two inputs
// using the 'diff' tool.
package diff

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
)

// Returns diff of two arrays of bytes in diff tool format.
func Diff(oldName string, old []byte, newName string, new []byte) ([]byte, error) {
	f1, err := writeTempFile(old)
	if err != nil {
		return nil, err
	}
	defer os.Remove(f1)

	f2, err := writeTempFile(new)
	if err != nil {
		return nil, err
	}
	defer os.Remove(f2)

	data, err := exec.Command("diff", "-u", f1, f2).CombinedOutput()
	if err != nil && len(data) == 0 {
		return nil, err
	}

	if len(data) == 0 {
		return nil, nil
	}

	i := bytes.IndexByte(data, '\n')
	if i < 0 {
		return data, nil
	}
	j := bytes.IndexByte(data[i+1:], '\n')
	if j < 0 {
		return data, nil
	}
	start := i + 1 + j + 1
	if start >= len(data) || data[start] != '@' {
		return data, nil
	}

	return append([]byte(fmt.Sprintf("diff %s %s\n--- %s\n+++ %s\n", oldName, newName, oldName, newName)), data[start:]...), nil
}

func writeTempFile(data []byte) (string, error) {
	file, err := ioutil.TempFile("", "rf-diff")
	if err != nil {
		return "", err
	}
	_, err = file.Write(data)
	if err1 := file.Close(); err == nil {
		err = err1
	}
	if err != nil {
		os.Remove(file.Name())
		return "", err
	}
	return file.Name(), nil
}
