======================================
Planck: Parser LANguage Combinator Kit
======================================

Tiny Parsec in OCaml. 

Version 0.1.0. 

Requirements
============

What you require:

* ocaml 3.12.0
* ocamlfind
* type-conv 2.3.0 and sexplib 5.2.1 -- from http://ocaml.janestreet.com/?q=node/13
* spotlib -- from http://bitbucket.org/camlspotter/spotlib/

The followings are required for building an ocaml parser using Planck

* pa_monad_custom -- from http://bitbucket.org/camlspotter/pa_monad_custom/
* ocaml 3.12.0 source code tree, compiled, and its toplevel directory is symlinked as `ocaml/ocaml' in this directory. I mean, cat ocaml/ocaml/VERSION must show you a text "3.12.1...".
* (optional) lablgtk-2.14.2 source code for test parsing of oo codes. Its toplevel directory must be symlinked as `lablgtk-2.14.2' in this directory.

If you are not interesting in preparing these things for the ocaml parser, edit around the last line of OMakefile.

Install
=======

1. set PREFIX env var
2. yes no | omake --install
3. omake

Modules
=======

* Result	: Result monad. Haskell's Either monad, but I hate the names.

* Position	: Module for locations like bytes, lines and columns of inputs.

* Lazylist	: Used for implementing parse target streams

* Stream_intf	: Module type declarations for streams.
* Stream	: Basic stream operations
* Sstring	: Stream of strings (Not chars) 
* Sbuffer	: Stream specialized for chars with efficient buffering
* Smemo		: Stream with memoization by stream positions

* Planck_intf	: Module type declarations for parser combinators
* Pbase		: Base parser combinators
* Pchar         : Parser combinators specialized for char stream
* Pbuffer	: Parser combinators specialized for Sbuffer

* Op_prec	: Operator precedence resolution

Test
====

* expr          : This is a small calculator. It contains:
                  
                    * binary operator precedence resolution
                    * manual removal of left recursion rules

                  To test its parsing and calculation correctness, cd test; omake test .
