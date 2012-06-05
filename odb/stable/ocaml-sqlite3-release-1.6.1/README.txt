SQLite3 bindings for Objective Caml
===================================

  Copyright (c) 2005 Christian Szegedy <csdontdpam871@metamatix.org>

  Copyright (c) 2007 Jane Street Holding, LLC
                     1 New York Plaza, 33rd Floor
                     New York, NY 10004
                     USA
                     Author: Markus Mottl <markus.mottl@gmail.com>

  Copyright (c) 2007 Enrico Tassi <gareuselesinge@virgilio.it>

DESCRIPTION:

  * SQLite 3 database library wrapper for OCaml.

  * SQLite 3 is available from http://www.sqlite.org

  * This wrapper is written in a way that enables a friendly coexistence
    with the old (version 2) sqlite and its OCaml wrapper ocaml-sqlite.

REQUIREMENTS:

  * OCaml 3.11 or above

  * ocamlfind

  * SQLite library (tested on 3.3.3 version)

    * libsqlite3.a installed in /usr/lib or /usr/local/lib. If it
      is installed somewhere else, modify configure.ac accordingly.

    * sqlite3.h installed in /usr/include or /usr/local/include.
      If it is installed anywhere else, modify configure.ac accordingly.

    * For versions other than 3.3.3 the wrapping of error codes should
      be checked.

INSTALLATION:

  To build and install the bindings:

    ./configure   # Makefile & META files
    make bytecode # bytecode library
    make opt      # native library
    make doc      # documentation
    make all      # both native and bytecode
    make install  # install
    make remove   # uninstall

  To build the test examples:

    cd test

    # execute one of:

    ocamlfind ocamlopt -package sqlite3 -linkpkg test_db.ml
    ocamlfind ocamlopt -package sqlite3 -linkpkg test_exec.ml
    ocamlfind ocamlopt -package sqlite3 -linkpkg test_stmt.ml
    ocamlfind ocamlopt -package sqlite3 -linkpkg test_fun.ml

LICENSE:

  The package is released under the MIT license, see COPYING file for
  details.

CREDITS:

  Enrico Tassi contributed support for user-defined scalar functions.

  Markus Mottl rewrote Christian's bindings for Jane Street Holding to
  clean up a few things and to make it perform better in multi-threaded
  environments.

  Christian Szegedy wrote the initial release for SQLite version 3.

  Mikhail Fedotov wrote ocaml-sqlite for SQLite version 2.
  His binding served as a reference for this wrapper, but sqlite3
  is written completely from scratch since the C interface changed
  significantly.

LINKS:

  http://caml.inria.fr - OCaml language
  http://www.sqlite.org - SQLite library
