// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Git-generate regenerates a commit from a script kept in the commit message.
//
// Specifically, the topmost commit in the current git repo should have a script
// preceded by [git-generate] on a line by itself. The script continues until the
// end of the message or a Change-Id: line. The script starts execution in the
// root of the git repo.
//
// For example, a commit message might say:
//
// 	We are moving from Old to New.
//
// 	[git-generate]
// 	cd some/dir
// 	sed -i '' 's/Old/New/g' *
//
// To regenerate the commit, git-generate resets the working file state to before
// the commit and then runs the script. The script runs using 'bash -e', so any
// single command failing will abort the generation.
//
// When a merge conflict occurs while rebasing, git does not stop with the
// conflicting commit at the top, so git-generate will not find it by default.
// The -conflict flag tells git-generate to look for the script in the upcoming
// (conflicting) commit.
package main

import (
	"bytes"
	"flag"
	"fmt"
	"io/fs"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

func usage() {
	fmt.Fprintf(os.Stderr, "usage: git-generate [-conflict] [-f file]\n")
	fmt.Fprintf(os.Stderr, "See 'go doc rsc.io/rf/git-generate' for details.\n")
	os.Exit(2)
}

var (
	conflict = flag.Bool("conflict", false, "generate from script in REBASE_HEAD to resolve conflict")
	file     = flag.String("f", "", "read commit message containing script from `file`")
)

func main() {
	log.SetFlags(0)
	log.SetPrefix("git-generate: ")
	flag.Usage = usage
	flag.Parse()
	if flag.NArg() != 0 {
		flag.Usage()
	}

	gitdir := strings.TrimSpace(git("rev-parse", "--show-toplevel"))

	if *file != "" && *conflict {
		log.Fatalf("cannot use -conflict with -f")
	}

	what := "HEAD"
	if *conflict {
		if _, err := os.Stat(gitdir); err != nil {
			log.Fatalf("cannot find git directory")
		}
		if _, err := os.Stat(filepath.Join(gitdir, ".git/rebase-merge/stopped-sha")); err == nil {
			what = "REBASE_HEAD"
		} else if _, err := os.Stat(filepath.Join(gitdir, ".git/CHERRY_PICK_HEAD")); err == nil {
			what = "CHERRY_PICK_HEAD"
		} else {
			log.Fatalf("cannot find rebase or cherry-pick conflict in progress")
		}
		log.Printf("using %s to resolve conflict", what)
	}

	var script []string
	if *file != "" {
		what = *file
		data, err := ioutil.ReadFile(*file)
		if err != nil {
			log.Fatal(err)
		}
		msg := string(data)
		// Allow file to be indented four spaces (output of git log)
		// or unintended (what you'd write in an actual commit message edit).
		script = readScript(msg, "    ")
		if script == nil {
			script = readScript(msg, "")
		}
	} else {
		msg := git("log", "-n1", what)
		script = readScript(msg, "    ")
	}

	if len(script) == 0 {
		log.Fatalf("no script found in %s", what)
	}

	if what == "HEAD" && *file == "" {
		// Reset files to HEAD^ in preparation for reapplying HEAD commit.
		// For a brief moment HEAD is pointing at HEAD^, which is unfortunate.
		// It would be very nice if there were some single command to do this pair,
		// which is to say, to update the working directory but not the branch head.
		// Checkout followed by the removal loop, as in the else case below,
		// is not enough because it does not remove files that are added in HEAD.
		// (git status thinks they are fine!)
		// Perhaps we can list them some other way.
		now := strings.TrimSpace(git("rev-parse", "HEAD"))
		git("reset", "-q", "--hard", "HEAD^")
		git("reset", "-q", now)
	} else {
		// Applying script on top of HEAD itself.
		gitDir(gitdir, "checkout", "HEAD", ".")

		// Checkout doesn't remove files that have been git added,
		// such as those that might be left over from a conflicting cherry-pick or merge.
		// Remove them ourselves.
		for _, line := range strings.Split(gitDir(gitdir, "status", "--porcelain=1"), "\n") {
			if len(line) >= 4 {
				switch line[0:2] {
				case "AD", "A ", "UU", "DU":
					name := filepath.Join(gitdir, line[3:])
					os.Remove(name)
					if _, err := os.Stat(name); err == nil {
						log.Fatalf("unable to remove %s", name)
					}
					os.Remove(filepath.Dir(name)) // in case directory is now empty: NOT RemoveAll
				}
			}
		}
	}

	have := make(map[string]bool)
	walkGit(gitdir, func(path string) {
		have[path] = true
	})

	f, err := ioutil.TempFile("", "git-generate-")
	if err != nil {
		log.Fatal(err)
	}
	f.Write([]byte(strings.Join(script, "")))
	f.Close()
	cmd := exec.Command("bash", "-e", f.Name())
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	cmd.Dir = gitdir
	err = cmd.Run()
	os.Remove(f.Name())
	if err != nil {
		log.Fatalf("executing script: %v", err)
	}

	addCmd := []string{"add", "-N"}
	walkGit(gitdir, func(path string) {
		if !have[path] {
			addCmd = append(addCmd, path)
		}
	})
	if len(addCmd) > 2 {
		gitDir(gitdir, addCmd...)
	}
	git("add", "-u")
}

func readScript(msg, prefix string) []string {
	lines := strings.SplitAfter(msg, "\n")
	var script []string
	for _, line := range lines {
		if !strings.HasPrefix(line, prefix) {
			continue
		}
		line = line[len(prefix):]
		if script == nil && line == "[git-generate]\n" {
			script = []string{}
			continue
		}
		if strings.HasPrefix(line, "Change-Id:") {
			break
		}
		if len(script) == 0 && strings.TrimSpace(line) == "" {
			continue
		}
		if script != nil {
			script = append(script, line)
		}
	}
	return script
}

func git(args ...string) string {
	return gitDir(".", args...)
}

func gitDir(dir string, args ...string) string {
	cmd := exec.Command("git", args...)
	cmd.Dir = dir
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	out, err := cmd.Output()
	if err != nil {
		log.Fatalf("git %s: %v\n%s%s", strings.Join(args, " "), err, stderr.Bytes(), out)
	}
	return string(out)
}

func walkGit(dir string, f func(path string)) {
	filepath.WalkDir(dir, func(path string, d fs.DirEntry, err error) error {
		if d.Name() == ".git" {
			return fs.SkipDir
		}
		if d.IsDir() {
			return nil
		}
		rel := strings.TrimPrefix(path, dir+string(filepath.Separator))
		if rel == path {
			log.Fatalf("cannot compute relative path: %s vs %s", dir, path)
		}
		f(rel)
		return nil
	})
}
