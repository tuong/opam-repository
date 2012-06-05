======================================
OCaml parser by Planck
======================================

Parser for OCaml writen in Planck

Requirements
============

What you require:

* pa_monad_custom -- from http://bitbucket.org/camlspotter/pa_monad_custom/
* Spotlib -- from http://bitbucket.org/camlspotter/spotlib/
* Planck
* ocaml 3.12.0 source code tree, compiled, and its toplevel directory is symlinked as `ocaml' in this directory.
* (optional) lablgtk-2.14.2 source code for test parsing of oo codes. Its toplevel directory must be symlinked as `lablgtk-2.14.2' in this directory.

What you can try
================

./parser <ocaml source ml/mli>

It checks the parsed result with the one of the original ocaml parser. If any difference is found, it raises an uncaught exception.
