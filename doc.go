// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Rf refactors Go programs.
//
// Usage:
//
//	rf [-diff] script
//
// Rf applies a script of refactoring commands to the package in the current directory.
// For example, to unexport a field in a struct by renaming it:
//
//	rf 'mv T.Field T.field'
//
// By default, rf writes changes back to the disk.
// The -diff flag causes rf to print a diff of the intended changes instead.
//
// A script is a sequence of commands, one per line.
// Comments are introduced by # and extend to the end of the line.
// Commands may be broken across lines by ending all but the last
// with a trailing backslash (before any comment), as in:
//
//	rf '
//		# command
//		mv T.Field \ # source
//		   T.field   # destination
//	'
//
// Commands that take { } blocks need not backslash-escape line breaks inside the braces, as in:
//
//	rf '
//		ex {
//			var x *Node
//			x.Left -> x.GetLeft()
//		}
//	'
//
// # Code addresses
//
// Most commands take “code addresses” as arguments.
// Each code address identifies some code in a program.
// For illustration, consider this program, prog.go:
//
//	package p
//
//	import (
//		"fmt"
//		"io"
//	)
//
//	const (
//		C = iota
//		D
//	)
//
//	const E = 2.718281828
//
//	func F(w io.Writer) {
//		who := "world"
//		msg := fmt.Sprintf("hello, %v", who)
//		fmt.Fprintf(w, "%s\n", msg)
//	}
//
//	type T struct { Field int }
//
//	func (t T) M() string {
//		return fmt.Sprint(t)
//	}
//
//	func (*T) P() {}
//
//	type TAlias = T
//
//	var V struct {
//		Value int
//		thing T
//	}
//
//	var VT T
//
// The simplest code address is the name of a top-level declaration.
// In this program, those addresses are C, D, F, T, TAlias, V, and VT.
//
// Adding .Name to an address selects a name within the earlier address,
// whether that's a function variable (F.who, F.msg),
// a struct field (T.Field), a method (T.M, T.P),
// or a variable's field (V.Value, V.thing, V.thing.Field, VT.Field).
//
// Another kind of code address is a Go source file, identified by
// a name ending in “.go”, as in “file.go”. If the file name contains
// a slash, as in “../dir/file.go”, the address identifies a file in
// the package in “../dir”.
//
// Another kind of code address is a Go package, identified by a path
// containing a slash but not ending in “.go”, as in “../dir” or
// “example.com/pkg”.
//
// A final kind of code address is a textual range in a function body
// or source file, identified by the syntax Ident:Range, where Ident
// identifies a file, function, or method and Range identifies a
// section of text within. The Range syntax is as used in the
// Acme and Sam text editors. The most common forms are the line range
// “N,M”, the byte range “#N,M”, and the regular expression range
// “/re1/,/re2/”. For example:
//
//	prog.go:9                 # "world" line
//	prog.go:9,10              # "world" and fmt.Sprintf lines
//	prog.go:/msg/-0,/fmt/+0   # "world" and fmt.Sprintf lines
//	F:/msg/-0,/fmt/+0         # (same)
//	prog.go:/fmt.*/           # the fmt.Sprintf call in F
//	F:/fmt.Sprint.*/          # (same)
//	T.M:/fmt.*/               # the fmt.Sprint call in T.M
//
// See http://9p.io/sys/doc/sam/sam.html Table II for details on the syntax.
//
// # The add command
//
// The add command adds text to the source code. It takes as an argument the
// address after which the text should be added, followed by the text itself.
//
//	add address text...
//
// The address may be a declaration, text range, file, or package.
// In all cases, the text is added immediately after the addressed location:
// after the declaration, after the text range, at the end of the file,
// or at the end of the first file in the package (considering the file names
// in lexical order).
//
// Examples:
//
//	add T:$ \
//		NewField int `tag`
//
//	add x.go func F() {}
//
// # The cp command
//
// The cp command is like mv (see below) but doesn't delete the source
// and doesn't update any references. (UNIMPLEMENTED)
//
// # The ex command
//
// The ex command applies rewrites based on example snippets.
//
//	ex { [imports] [declarations] old->new... }
//
// The arguments to ex are interpreted as Go code consisting of a
// sequence of imports and then a list of special “old -> new” rules.
// Each rule specifies that where ex finds a pattern matching old,
// it should replace the code with new. For example, to replace all
// log.Error calls with log.Panic:
//
//	ex { import "log"; log.Error -> log.Panic }
//
// Declarations introduce typed pattern variables that can be used
// in rules. For example, to simplify certain needlessly complex
// uses of fmt.Sprintf:
//
//	ex {
//		import "fmt";
//		import "strconv";
//		var s string;
//		fmt.Sprintf("%s", s) -> s;
//		fmt.Sprintf("%v", s) -> s;
//		fmt.Sprintf("%q", s) -> strconv.Quote(s)
//	}
//
// # The inline command
//
// The inline command inlines uses of declared constants, functions, and types.
//
//	inline [-rm] decl...
//
// Each use of the named declarations is replaced by the declaration's definition.
// If the -rm flag is given, inline removes the declarations as well.
//
// Examples:
//
//	inline E
//
//	inline -rm TAlias
//
// Given the declarations in the “Code addresses” section above, the first command
// replaces all uses of E with 2.718281828. The second replaces all uses of
// TAlias with T and then removes TAlias.
//
// UNIMPLEMENTED: Inlining of functions.
//
// # The key command
//
// The key command converts all struct literals for a list of types to keyed literals.
//
//	key address...
//
// Each address must identify a struct type. All literals of those struct types are
// updated to use the keyed form.
//
// Example:
//
//	key Point
//
// # The mv command
//
// The mv command moves and renames code.
//
//	mv [-f] old... new
//
// When mv moves or renames old code, it also updates any references
// to use the new names or locations for the code.
// This includes updating other packages in the current module.
//
// In general, mv aims to act appropriately for any sensible
// combination of old and new address form.
// The rest of this section enumerates the specific cases that
// mv handles.
//
// item → renamed item
//
// Any named item can be renamed by specifying a destination
// that is the same code address with the final element changed.
// For example:
//
//	mv E Euler             # constant
//	mv F Func              # function
//	mv F.w F.writer        # argument name
//	mv F.who F.greetee     # local variable name
//	mv T MyType            # type
//	mv T.M T.Method        # method
//	mv T.M.t T.M.rcvr      # receiver name
//	mv Point.x Point.X     # struct field
//	mv V Var               # var
//	mv V.Value V.IntValue  # var struct field
//
// In this form, the destination address must repeat the dot-separated elements
// leading up to the new name, changing only the final element.
// The repetition here distinguishes this form from other forms.
//
// var → var field
//
// A top-level variable can be moved to a new or existing field in a
// global variable of struct type. For example:
//
//	mv VT V.VT
//
// method → func
//
// A method can be moved to a top-level function, removing the association with the receiver type.
// The receiver remains the first argument of the new function. For example:
//
//	mv T.Method TFunction
//
// func → method
//
// A function can be moved to a method on the type of its first argument,
// assuming that type is defined in the same package where the function appears.
// For example:
//
//	mv TFunction T.Method
//
// code text → new function
//
// A text range can be moved to a new function, leaving behind an appropriate
// call to that function. For example:
//
//	mv F:/msg/,$ Greet
//
// code text → new method
//
// A text range can be moved to a new method, leaving behind an approriate
// call to that method. For example: TODO.
//
// UNIMPLEMENTED.
//
// declaration → file
//
// If the source is a top-level declaration (const, func, method, type, var)
// and the destination is a file, mv moves that declaration, along with
// any comments immediately preceding it, to the end of the
// destination file. For example:
//
//	mv Template NewTemplate Template.Method thing.go
//
// Naming a single item in a declaration block is taken to indicate
// wanting to move all items in the block. In the example from the
// “Code addresses” section, “mv C x.go” moves D as well.
//
// Any time a destination file must be created, mv initializes it
// with the header comments (those above the package declaration
// and any package doc) from the file the source code is being
// moved from. This heuristic is meant to copy header text like copyright notices.
//
// The file may be in a different package. As usual, mv updates references
// to the moved declaration to refer to its new location, inserting imports
// as needed. If the result is an import cycle, mv reports the cycle
// rather than attempt some kind of automatic (and likely wrong) fix.
//
// declaration → package
//
// If the source is a top-level declaration and the destination is a package,
// mv moves the declaration to the a file in the destination package with
// the same name as the one holding the source item.
//
//	mv F ../otherpkg  # if F is in f.go, same as mv F ../otherpkg/f.go
//
// If the destination package does not exist, it will be created.
//
// file → file
//
// If the source and destination are both files, mv moves all code from
// the source file to the end of the destination file. For example:
//
//	mv x.go y.go
//	mv x.go ../otherpkg/y.go
//
// file → package
//
// If the source is a file and the destination is a package, mv moves all code
// from the source file to a file in the destination with the same base name.
// For example:
//
//	mv x.go ../otherpkg   # same as mv x.go ../otherpkg/x.go
//
// package → file
//
// If the source is a package and the destination is a file, mv moves all code
// from the source package to the named file.
// For example:
//
//	mv ../otherpkg x.go
//
// UNIMPLEMENTED.
//
// package → package
//
// If the source is a package and the destination is a file, mv moves all code
// from the source package to the destination package.
// from the source package to the named file.
// For example:
//
//	mv ../otherpkg x.go
//
// UNIMPLEMENTED.
//
// many → file, many → package
//
// If the destination is a file or package, multiple sources can be listed.
// The mv command moves each source item in turn to the destination.
//
// # The rm command
//
// The rm command removes code.
//
//	rm old...
//
// Rm deletes the old code. All address forms are valid.
// When deleting a declaration, rm also deletes line comments
// immediately preceding it, up to a blank line.
//
// # Bugs Bugs Bugs
//
// Rf is very very rough. Everything is subject to change, and it may break your programs.
package main
