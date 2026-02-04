// Copyright 2023 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package refactor

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"sync"
)

type Config struct {
	// BuildTags is a list of build tags to set for this configuration.
	//
	// Some build tags are propagated specially:
	//
	// - GOOS and GOARCH build tags control the GOOS/GOARCH environment
	// variables.
	//
	// - The "race" build tag controls the -race flag.
	//
	// - The "cgo" and "!cgo" build tags control the CGO_ENABLED environment
	// variable.
	//
	// TODO: We might need to do something for release tags, too.
	BuildTags []string
}

type Configs struct {
	c []Config
}

func readJSON(cmd *exec.Cmd, out any) error {
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = os.Stderr
	err := cmd.Run()
	if err != nil {
		return err
	}
	return json.Unmarshal(stdout.Bytes(), out)
}

type goosGoarch struct {
	GOOS         string
	GOARCH       string
	CgoSupported bool
}

var platformsOnce struct {
	once sync.Once
	ps   []goosGoarch
	err  error
}

func platforms(goBinary string) ([]goosGoarch, error) {
	platformsOnce.once.Do(func() {
		var platforms []goosGoarch
		cmd := exec.Command(goBinary, "tool", "dist", "list", "-json")
		if err := readJSON(cmd, &platforms); err != nil {
			platformsOnce.err = fmt.Errorf("getting GOOS/GOARCH values: %w", err)
			return
		}
		platformsOnce.ps = platforms
	})
	return platformsOnce.ps, platformsOnce.err
}

// NewConfigs returns a new Configs consisting of one Config with the given
// build tags.
func NewConfigs(buildTags ...string) Configs {
	return Configs{[]Config{{buildTags}}}
}

func (cs Configs) Cross(cs2 Configs) Configs {
	out := Configs{make([]Config, 0, len(cs.c)*len(cs2.c))}
	cross := func(c1, c2 Config) Config {
		return Config{append(append([]string(nil), c1.BuildTags...), c2.BuildTags...)}
	}
	for _, c1 := range cs.c {
		for _, c2 := range cs2.c {
			out.c = append(out.c, cross(c1, c2))
		}
	}
	return out
}

func (cs Configs) Plus(cs2 Configs) Configs {
	return Configs{append(append([]Config(nil), cs.c...), cs2.c...)}
}

// ConfigsAllPlatforms returns a Configs for all GOOS/GOARCH combinations,
// including covering both cgo and non-cgo where possible.
func ConfigsAllPlatforms(goBinary string) (Configs, error) {
	plats, err := platforms(goBinary)
	if err != nil {
		return Configs{}, err
	}

	var goEnv struct {
		GOOS, GOARCH string
	}
	cmd := exec.Command("go", "env", "-json", "GOOS", "GOARCH")
	if err := readJSON(cmd, &goEnv); err != nil {
		return Configs{}, err
	}

	var cs Configs
	for _, plat := range plats {
		c := NewConfigs(plat.GOOS, plat.GOARCH)
		// In general, we can only process cgo for our host platform since it
		// requires invoking the C toolchain. On other platforms, take the
		// toolchain default. Hopefully between {cgo,!cgo} on the host platform,
		// and the default on all other platforms, we get all of the source
		// files.
		if plat.CgoSupported && goEnv.GOOS == plat.GOOS && goEnv.GOARCH == plat.GOARCH {
			cgos := NewConfigs("cgo").Plus(NewConfigs("!cgo"))
			c = c.Cross(cgos)
		}
		cs = cs.Plus(c)
	}
	return cs, nil
}

func (c Config) String() string {
	return strings.Join(c.BuildTags, ",")
}

// flagsEnvs returns the flags and environment variables to pass to go build to
// produce this build configuration.
func (c Config) flagsEnvs(goBinary string) (flags, envs []string, err error) {
	plats, err := platforms(goBinary)
	if err != nil {
		return nil, nil, err
	}
	gooses := make(map[string]bool)
	goarches := make(map[string]bool)
	for _, plat := range plats {
		gooses[plat.GOOS] = true
		goarches[plat.GOARCH] = true
	}

	var flagTags []string
	haveEnv := make(map[string]string)
	addEnv := func(k, v string) error {
		if v2, ok := haveEnv[k]; ok {
			if v == v2 {
				return nil
			}
			return fmt.Errorf("conflicting %s values: %s and %s", k, v, v2)
		}
		haveEnv[k] = v
		envs = append(envs, k+"="+v)
		return nil
	}
	for _, tag := range c.BuildTags {
		switch {
		case gooses[tag]:
			if err := addEnv("GOOS", tag); err != nil {
				return nil, nil, err
			}
		case goarches[tag]:
			if err := addEnv("GOARCH", tag); err != nil {
				return nil, nil, err
			}
		case tag == "cgo":
			if err := addEnv("CGO_ENABLED", "1"); err != nil {
				return nil, nil, err
			}
		case tag == "!cgo":
			if err := addEnv("CGO_ENABLED", "0"); err != nil {
				return nil, nil, err
			}
		case tag == "race":
			flags = append(flags, "-race")
		default:
			flagTags = append(flagTags, tag)
		}
	}
	if len(flagTags) > 0 {
		flags = append(flags, "-tags="+strings.Join(flagTags, ","))
	}
	return flags, envs, nil
}
