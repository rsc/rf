// Copyright 2012 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package refactor

import (
	"errors"
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"log"
	"reflect"
	"regexp"
	"strconv"
	"strings"
	"unicode/utf8"
)

type Item struct {
	Kind  ItemKind
	Name  string
	Outer *Item
	Obj   types.Object
	Pos   token.Pos
	End   token.Pos
}

func (i *Item) Outermost() *Item {
	for i != nil && i.Outer != nil {
		i = i.Outer
	}
	return i
}

type ItemKind int

const (
	_ ItemKind = iota
	ItemNotFound
	ItemFile
	ItemDir
	ItemConst
	ItemType
	ItemVar
	ItemFunc
	ItemField
	ItemMethod
	ItemPos
	ItemPkg
)

func (k ItemKind) String() string {
	switch k {
	case ItemNotFound:
		return "not found"
	case ItemFile:
		return "file"
	case ItemDir:
		return "dir"
	case ItemConst:
		return "const"
	case ItemType:
		return "type"
	case ItemVar:
		return "var"
	case ItemFunc:
		return "func"
	case ItemField:
		return "field"
	case ItemMethod:
		return "method"
	case ItemPos:
		return "text"
	case ItemPkg:
		return "pkg"
	}
	return "???"
}

func (s *Snapshot) EvalNext(args string) (*Item, string, string) {
	args = strings.TrimLeft(args, " \t\n")
	if args == "" {
		return nil, "", ""
	}
	expr := args
	for i := 0; i < len(expr); i++ {
		switch expr[i] {
		case ' ', '\t', '\n':
			expr, rest := expr[:i], args[i+1:]
			return s.Eval(expr), expr, rest
		case ':':
			// Scan address.
			slash := false
			expr, addr := expr[:i], expr[i+1:]
			var rest string
			for j := 0; j < len(addr); j++ {
				switch addr[j] {
				case ' ', '\t', '\n':
					if !slash {
						addr, rest = addr[:j], addr[j:]
					}
				case '/':
					slash = !slash
				case '\\':
					if slash {
						j++
					}
				}
			}
			outer := expr
			expr = outer + ":" + addr
			if slash {
				s.ErrorAt(token.NoPos, "unterminated text range: %v", expr)
				return nil, expr, ""
			}
			item := s.Eval(outer)
			if item == nil {
				return nil, expr, ""
			}
			var start, end token.Pos
			afterBrace := false
			switch item.Kind {
			default:
				s.ErrorAt(token.NoPos, "cannot apply text range to %v", item.Kind)
				return nil, expr, ""

			case ItemFunc, ItemMethod:
				fvar := item.Obj
				stack := s.SyntaxAt(fvar.Pos()) // FuncType Ident FuncDecl
				decl := stack[2].(*ast.FuncDecl)
				start, end = decl.Body.Lbrace+1, decl.Body.Rbrace
				afterBrace = true

			case ItemVar, ItemType:
				tvar := item.Obj
				typ := tvar.Type().Underlying()
				if ptr, ok := typ.(*types.Pointer); ok {
					typ = ptr.Elem().Underlying()
				}
				switch typ.(type) {
				default:
					s.ErrorAt(token.NoPos, "cannot apply text range to %v %v", item.Kind, typ)
					return nil, expr, ""
				case *types.Struct:
					// Finding struct type is a little tricky.
					// Except for empty structs, we could use the pos of the first field.
					// But an empty struct type has no pos at all, so we have to
					// use the pos of the declaration in which the struct appears.
					var structPos token.Pos
					switch typ := tvar.Type().(type) {
					case *types.Struct:
						structPos = tvar.Pos()
					case *types.Named:
						structPos = typ.Obj().Pos()
					}
					stack := s.SyntaxAt(structPos) // Ident TypeSpec GenDecl or Ident ValueSpec GenDecl
					var struc *ast.StructType
					switch spec := stack[1].(type) {
					case *ast.TypeSpec:
						struc = spec.Type.(*ast.StructType)
					case *ast.ValueSpec:
						struc = spec.Type.(*ast.StructType)
					}
					start, end = struc.Fields.Opening+1, struc.Fields.Closing
					afterBrace = true
				}

			case ItemFile:
				_, f := s.FileByName(item.Name)
				if f == nil {
					s.ErrorAt(token.NoPos, "cannot evaluate address %s: file not found: %s", addr, item.Name)
					return item, "", ""
				}
				start, end = s.FileRange(f.Package)
			}

			text := s.Text(start, end)
			if afterBrace {
				i := 0
				for i < len(text) && (text[i] == ' ' || text[i] == '\t') {
					i++
				}
				if i < len(text) && text[i] == '\n' {
					start += token.Pos(i + 1)
					text = text[i+1:]
				}
			}
			lo, hi, err := addrToByteRange(addr, 0, text)
			if err != nil {
				s.ErrorAt(token.NoPos, "cannot evaluate address %s: %v", addr, err)
			}
			item = &Item{
				Kind:  ItemPos,
				Outer: item,
				Pos:   start + token.Pos(lo),
				End:   start + token.Pos(hi),
			}
			return item, expr, rest
		}
	}
	return s.Eval(expr), expr, ""
}

func (s *Snapshot) EvalList(args string) ([]*Item, []string) {
	var items []*Item
	var exprs []string
	for {
		item, expr, rest := s.EvalNext(args)
		if expr == "" {
			break
		}
		items = append(items, item)
		exprs = append(exprs, expr)
		args = rest
	}
	return items, exprs
}

func (s *Snapshot) Eval(expr string) *Item {
	if strings.HasSuffix(expr, ".go") {
		return &Item{Kind: ItemFile, Name: expr}
	}
	if expr == "." || strings.Contains(expr, "/") {
		return &Item{Kind: ItemDir, Name: expr}
	}

	name, rest, more := cut(expr, ".")
	item := evalScope(s.target.Types.Scope(), name)
	item.Name = name
	p := s.target
	if item.Kind == ItemNotFound {
		// Try for package name
		for _, file := range s.target.Files {
			if file.Syntax == nil {
				continue
			}
			scope := s.target.TypesInfo.Scopes[file.Syntax]
			if scope == nil {
				panic("no file scope?")
			}
			pkg := evalScope(scope, name)
			pkg.Name = name
			if pkg.Kind == ItemPkg {
				if !more {
					fmt.Println("HAVE", pkg)
					return pkg
				}
				name, rest, more = cut(rest, ".")
				tpkg := pkg.Obj.(*types.PkgName).Imported()
				for _, pp := range s.packages {
					if pp.Types == tpkg {
						p = pp
						break
					}
				}
				item = evalScope(tpkg.Scope(), name)
				item.Outer = pkg
				item.Name = pkg.Name + "." + name
				break
			}
		}
	}
	if item.Kind == ItemNotFound {
		return item
	}
	for more {
		name, rest, more = cut(rest, ".")
		item = evalPackage(p, item, name)
		if item.Kind == ItemNotFound {
			return item
		}
	}
	return item
}

func evalScope(scope *types.Scope, expr string) *Item {
	obj := scope.Lookup(expr)
	switch obj := obj.(type) {
	default:
		log.Fatalf("%s is a %T, unimplemented", expr, obj)
		return nil
	case nil:
		return &Item{Kind: ItemNotFound, Name: expr}
	case *types.TypeName:
		return &Item{Kind: ItemType, Obj: obj}
	case *types.Const:
		return &Item{Kind: ItemConst, Obj: obj}
	case *types.Var:
		return &Item{Kind: ItemVar, Obj: obj}
	case *types.Func:
		return &Item{Kind: ItemFunc, Obj: obj}
	case *types.PkgName:
		return &Item{Kind: ItemPkg, Obj: obj}
	}
}

func evalPackage(p *Package, outer *Item, name string) *Item {
	switch outer.Kind {
	case ItemType:
		// Look for method, field.
		return lookupTypeX(p, outer, outer.Obj.Type(), name)
	case ItemVar:
		// If unnamed struct or interface, look in type.
		typ := outer.Obj.Type().Underlying()
		if ptr, ok := typ.(*types.Pointer); ok {
			typ = ptr.Elem().Underlying()
		}
		switch typ := typ.(type) {
		default:
			fmt.Printf("LOOKUP IN %T\n", typ)
		case *types.Struct, *types.Interface:
			return lookupTypeX(p, outer, typ, name)
		}
	case ItemFunc, ItemMethod:
		// Look for declaration inside function.
		item := evalScope(outer.Obj.(*types.Func).Scope(), name)
		item.Outer = outer
		item.Name = outer.Name + "." + name
		return item
	case ItemField:
		return lookupTypeX(p, outer, outer.Obj.Type(), name)
	}
	return &Item{Kind: ItemNotFound, Outer: outer, Name: outer.Name + "." + name}
}

// Code below copied from golang.org/x/tools/present/args.go.

// This file is stolen from go/src/cmd/godoc/codewalk.go.
// It's an evaluator for the file address syntax implemented by acme and sam,
// but using Go-native regular expressions.
// To keep things reasonably close, this version uses (?m:re) for all user-provided
// regular expressions. That is the only change to the code from codewalk.go.
// See http://9p.io/sys/doc/sam/sam.html Table II for details on the syntax.

// addrToByte evaluates the given address starting at offset start in data.
// It returns the lo and hi byte offset of the matched region within data.
func addrToByteRange(addr string, start int, data []byte) (lo, hi int, err error) {
	if addr == "" {
		lo, hi = start, len(data)
		return
	}
	var (
		dir        byte
		prevc      byte
		charOffset bool
	)
	lo = start
	hi = start
	for addr != "" && err == nil {
		c := addr[0]
		switch c {
		default:
			err = errors.New("invalid address syntax near " + string(c))
		case ',':
			if len(addr) == 1 {
				hi = len(data)
			} else {
				_, hi, err = addrToByteRange(addr[1:], hi, data)
			}
			return

		case '+', '-':
			if prevc == '+' || prevc == '-' {
				n := 1
				if prevc == '-' && c == '+' || prevc == '+' && c == '-' {
					// Don't bother with an out-of-range error for a
					// coordinate that's not going to be used.
					n = 0
				}
				lo, hi, err = addrNumber(data, lo, hi, prevc, n, charOffset)
			}
			dir = c

		case '$':
			lo = len(data)
			hi = len(data)
			if len(addr) > 1 {
				dir = '+'
			}

		case '#':
			charOffset = true

		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			var i int
			for i = 1; i < len(addr); i++ {
				if addr[i] < '0' || addr[i] > '9' {
					break
				}
			}
			var n int
			n, err = strconv.Atoi(addr[0:i])
			if err != nil {
				break
			}
			lo, hi, err = addrNumber(data, lo, hi, dir, n, charOffset)
			dir = 0
			charOffset = false
			prevc = c
			addr = addr[i:]
			continue

		// TODO: case '?' for backward matches?

		case '/':
			var i, j int
		Regexp:
			for i = 1; i < len(addr); i++ {
				switch addr[i] {
				case '\\':
					i++
				case '/':
					j = i + 1
					break Regexp
				}
			}
			if j == 0 {
				j = i
			}
			pattern := addr[1:i]
			lo, hi, err = addrRegexp(data, lo, hi, dir, pattern)
			dir = 0
			prevc = c
			addr = addr[j:]
			continue
		}
		prevc = c
		addr = addr[1:]
	}

	if err == nil && dir != 0 {
		lo, hi, err = addrNumber(data, lo, hi, dir, 1, charOffset)
	}
	if err != nil {
		return 0, 0, err
	}
	return lo, hi, nil
}

// addrNumber applies the given dir, n, and charOffset to the address lo, hi.
// dir is '+' or '-', n is the count, and charOffset is true if the syntax
// used was #n.  Applying +n (or +#n) means to advance n lines
// (or characters) after hi.  Applying -n (or -#n) means to back up n lines
// (or characters) before lo.
// The return value is the new lo, hi.
func addrNumber(data []byte, lo, hi int, dir byte, n int, charOffset bool) (newLo int, newHi int, err error) {
	switch dir {
	case 0:
		lo = 0
		hi = 0
		fallthrough

	case '+':
		if charOffset {
			pos := hi
			for ; n > 0 && pos < len(data); n-- {
				_, size := utf8.DecodeRune(data[pos:])
				pos += size
			}
			if n == 0 {
				return pos, pos, nil
			}
			break
		}
		// find next beginning of line
		if hi > 0 {
			for hi < len(data) && data[hi-1] != '\n' {
				hi++
			}
		}
		lo = hi
		if n == 0 {
			return lo, hi, nil
		}
		for ; hi < len(data); hi++ {
			if data[hi] != '\n' {
				continue
			}
			switch n--; n {
			case 1:
				lo = hi + 1
			case 0:
				return lo, hi + 1, nil
			}
		}

	case '-':
		if charOffset {
			// Scan backward for bytes that are not UTF-8 continuation bytes.
			pos := lo
			for ; pos > 0 && n > 0; pos-- {
				if data[pos]&0xc0 != 0x80 {
					n--
				}
			}
			if n == 0 {
				return pos, pos, nil
			}
			break
		}
		// find earlier beginning of line
		for lo > 0 && data[lo-1] != '\n' {
			lo--
		}
		hi = lo
		if n == 0 {
			return lo, hi, nil
		}
		for lo--; lo >= 0; lo-- {
			if lo > 0 && data[lo-1] != '\n' {
				continue
			}
			switch n--; n {
			case 1:
				hi = lo
			case 0:
				return lo, hi, nil
			}
		}
	}

	return 0, 0, errors.New("address out of range")
}

// addrRegexp searches for pattern in the given direction starting at lo, hi.
// The direction dir is '+' (search forward from hi) or '-' (search backward from lo).
// Backward searches are unimplemented.
func addrRegexp(data []byte, lo, hi int, dir byte, pattern string) (int, int, error) {
	// We want ^ and $ to work as in sam/acme, so use ?m.
	re, err := regexp.Compile("(?m:" + pattern + ")")
	if err != nil {
		return 0, 0, err
	}
	if dir == '-' {
		// Could implement reverse search using binary search
		// through file, but that seems like overkill.
		return 0, 0, errors.New("reverse search not implemented")
	}
	m := re.FindIndex(data[hi:])
	if len(m) > 0 {
		m[0] += hi
		m[1] += hi
	} else if hi > 0 {
		// No match.  Wrap to beginning of data.
		m = re.FindIndex(data)
	}
	if len(m) == 0 {
		return 0, 0, errors.New("no match for " + pattern)
	}
	return m[0], m[1], nil
}

func lookupTypeX(p *Package, outer *Item, typ types.Type, name string) *Item {
	if tn, ok := typ.(*types.Named); ok {
		n := tn.NumMethods()
		for i := 0; i < n; i++ {
			f := tn.Method(i)
			if f.Name() == name {
				return &Item{Kind: ItemMethod, Obj: f, Outer: outer, Name: outer.Name + "." + name}
			}
		}
		typ = tn.Underlying()
	}

	switch typ := typ.(type) {
	case *types.Interface:
		n := typ.NumMethods()
		for i := 0; i < n; i++ {
			f := typ.Method(i)
			if f.Name() == name {
				return &Item{Kind: ItemMethod, Obj: f, Outer: outer, Name: outer.Name + "." + name}
			}
		}

	case *types.Struct:
		n := typ.NumFields()
		for i := 0; i < n; i++ {
			f := typ.Field(i)
			if f.Name() == name {
				return &Item{Kind: ItemField, Obj: f, Outer: outer, Name: outer.Name + "." + name}
			}
		}
	}

	return &Item{Kind: ItemNotFound, Outer: outer, Name: outer.Name + "." + name}
}

func StackTypes(list []ast.Node) string {
	var types []reflect.Type
	for _, n := range list {
		types = append(types, reflect.TypeOf(n))
	}
	return fmt.Sprint(types)
}
