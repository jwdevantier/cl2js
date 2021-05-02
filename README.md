# cl2js

A (semi-)toy compiler from a sexp-syntax to javascript.

## Motivation
Primarily weekend hacking. Secondarily because [parenscript](https://common-lisp.net/project/parenscript/), which you should probably use, does not expose ES6 (and later) features yet.

This project 

## Overview

The intent is to create a [multi-pass compiler](https://en.wikipedia.org/wiki/Multi-pass_compiler) which expands the AST (possibly caching the result), then pass the resulting AST to a code-emitter which, given a set of emit-functions, translates forms to equivalent JS syntax.

The core AST-manipulation functions are in `ast.lisp` - user-supplied rules determine how the AST is rewritten and a convenient wrapper function allows supplying multiple sets of rules to rewrite the AST over multiple passes.

The core code emitter is inspired by [Pratical Common Lisp - Chapter 30 - An HTML Generation Library](http://www.gigamonkeys.com/book/practical-an-html-generation-library-the-interpreter.html) and lives in `emitter.lisp`.

The functions which generate the Javascript code are in `emitjs.lisp`, the core routines traverse an AST and emits code as dictated by a set of emit-functions using the code emitter.

The idea is to make the compiler simple to understand and easy to extend.

## Status
Very far from finished. Though it contains

* a decent code emitter inspired by 
* a function for rewriting an AST based on the supplied rules (expressed as functions)



## License

GPL-3.0-only

