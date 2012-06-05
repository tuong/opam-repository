---------------------------------------------------------------------------

                         Distribution of "LACAML"

---------------------------------------------------------------------------

                               Prerequisites

                               BLAS, LAPACK

             YOU WILL NEED GNU-MAKE TO COMPILE THE SYSTEM WITH
                         THE DISTRIBUTED MAKEFILES

---------------------------------------------------------------------------

                   Files in this distribution

Changelog            - History of code changes.

COPYRIGHT            - Copyright and authors of Lacaml.

INSTALL              - Short notes on compiling and installing the library

LICENSE              - A copy of the "GNU LESSER GENERAL PUBLIC LICENSE"

Makefile             - Global Makefile

OCamlMakefile        - Makefile for easy handling of compilation of not
                       so easy OCaml-projects.  It generates dependencies
                       of OCaml-files automatically, is able to handle
                       "ocamllex"-, "ocamlyacc"-, IDL- and C-files and
                       generates native- or byte-code, as executable or
                       as library - with thread-support if you want!

README.txt           - This file

examples/            - Various linear algebra examples.

lib/common.ml        - Globally visible utility-functions and definitions.

lib/float32.mli      - Modules that specialize for real/complex and
lib/float32.ml         single/double precision (globally visible through
lib/float64.mli        lacaml.ml).
lib/float64.ml
lib/complex32.mli
lib/complex32.ml
lib/complex64.mli
lib/complex64.ml

lib/impl_SDCZ.mli    - These implement the OCaml-interface to BLAS (or
lib/impl_SDCZ.ml       its more efficient ATLAS-implementation) and
lib/impl_SD.mli        LAPACK and only look like valid OCaml-files. In
lib/impl_SD.ml         fact four modules are generated from the SDCZ-files:
lib/impl_CZ.mli        two for single-precision operations, another
lib/impl_CZ.ml         two for double-precision, for real and complex
                       transforms respectively. The SD- and CZ-files handle
                       special functions that are only available for either
                       real or complex transformations.

lib/impl_SDCZ_c.c    - The C-interface to BLAS and LAPACK that works as a
lib/impl_SD_c.c        wrapper for the OCaml-interface, which cannot
lib/impl_CZ_c.c        interface FORTRAN directly.

lib/vec_SDCZ.mli     - Implementation of commonly useful vector operations.
lib/vec_SDCZ.ml
lib/vec_SD_c.c
lib/vec_CZ_c.c

lib/fold_col.c       - These C-files contain generic implementations of
lib/fold2_col.c        vector functions. They can be parameterized with
lib/vec_combine.c      macros for special cases.
lib/vec_map.c

lib/mat_SDCZ.mli     - Implementation of commonly useful matrix operations.
lib/mat_SDCZ.ml
lib/mat_SD.mli
lib/mat_SD.ml
lib/mat_CZ.mli
lib/mat_CZ.ml

lib/SD.ml            - Interfaces of the S, D, C, and Z submodules are
lib/SD.mli             generated from these by the post-configure script.
lib/CZ.ml
lib/CZ.mli

lib/lacaml_SDCZ.mli  - Interface template for the Lacaml library (the final
                       interface is generated in lacaml.mli by the post
                       configure script make_prec_dep.ml).

lib/utils.ml         - Internal auxiliary functions needed by other modules.

lib/impl_c.c         - Auxiliary C-routines needed in the C-interface.
lib/utils_c.c

lib/lacaml_macros.h  - C-header with macro definitions for writing
                       precision independent and convenient interfaces
                       to FORTRAN-libraries.

lib/myXerbla.f       - Dummy FORTRAN module for error handling that
                       overrides the one provided with BLAS and LAPACK:
                       these would otherwise abort execution of programs
                       on errors. Now we can handle errors in OCaml
                       instead.

---------------------------------------------------------------------------

                                 What is it?

This OCaml-library interfaces the BLAS-library (Basic Linear Algebra
Subroutines) and LAPACK-library (Linear Algebra routines), which are
written in FORTRAN.

This allows people to write high-performance numerical code for
applications that require linear algebra.

---------------------------------------------------------------------------

                            Why should you use it?

Here is a list of features:

  * The BLAS- and LAPACK-libraries have evolved over about two decades
    of time and are therefore extremely mature both in terms of stability
    and efficiency.

  * The OCaml-interface was designed in a way to combine both the
    possibility of gaining optimum efficiency (e.g. by allowing the
    creation of work arrays outside of loops) with simplicity (thanks
    to labels and default arguments).

  * The code is precision-independent and supports both real and complex
    transforms in a consistent way.  There are four modules that
    implement the same interface modulo the precision type and specialized
    real/complex functions.  If you refer to elements in this interface
    only, your code becomes precision- and (if meaningful) real/complex
    independent, too: you can choose at anytime whether you want to
    use single-precision or double-precision simply by "open"ing the
    required module.

  * You can fully exploit the library within multithreaded programs: most
    numerical routines are likely to run for a long time, but they will
    never block other threads.  This also means that you can execute
    several routines at the same time on several processors if you use
    native threads.

  * To make things easy for people used to the "real" implementation
    in FORTRAN but also for beginners who need detailed documentation,
    both function- and argument names have been kept compatible to the
    ones used in the BLAS- and LAPACK-documentation.  Only exception:
    you need not prefix functions with "s", "d", "c" or "z" to indicate
    the precision and type of numbers, because OCaml gives us more
    convenient means of choosing them.

---------------------------------------------------------------------------

                          General Usage Information

You can make use of this library by referring to the corresponding module you
need for your precision and number type:

  open Lacaml.S
  open Lacaml.D
  open Lacaml.C
  open Lacaml.Z

These modules become available if you link against the "lacaml"-library,
which is produced by this distribution.  You also need to link against the
"bigarray"-library provided by the OCaml-distribution and do not use findlib
to resolve dependencies automatically.  The "Lacaml.?"-modules implement the
BLAS/LAPACK-interface, and their corresponding submodules "Vec" and "Mat"
provide for vector and matrix operations that relate to the given precision
and number type.

Most functions allow optional arguments (= default arguments).  If you
do not provide them at the call-site, sane defaults will be used instead.

Convenient way of calling a function, e.g.:

  let rank = gelss in_mat out_mat in ...

This would perform computation of the solution to a general least squares
problem (= linear regression) with the SVD-algorithm using "in_mat"
as the matrix containing the predictor variables and "out_mat" as the
matrix containing (possibly many) response variables (this function can
handle several response variables at once).  The result is the rank of
the matrix, the matrices provided in the arguments will be overwritten
with further results (here: the singular vectors and the solution matrix).

If the above happened in a loop, this would be inefficient, because
a work-array would have to be allocated (and later deallocated) at
each call.  You can hoist the creation of this array out of the loop:

Efficient way of calling a function, e.g. (m, n, nrhs are problem
dependent parameters):

  let work = gelss_min_work ~m ~n ~nrhs in
  for i = 1 to 1000 do
    ...
    let rank = gelss in_mat ~work out_mat
    ...
  done

Take a look at file "lacaml.mli" (generated by the configure step) to
see which ways exists to pass parameters and to learn about the
defaults.

All matrices can be accessed in a restricted way, i.e. you can specify
submatrices for all matrix parameters.  E.g. if some matrix is called "a"
in the interface documentation, you can specify the left upper corner of
the wanted submatrix for the operation by setting "ar" for the row and
"ac" for the column (1 by default).  A vector "y" would have an extra
optional parameter "ofsy" (also 1 by default).

BLAS and LAPACK binary packages for Unix operating systems usually come
with appropriate man-pages.  E.g. to quickly look up how to factorize a
positive-definite, complex, single precision matrix, you might enter "man
cpotrf", and the function in Lacaml would be "Lacaml.C.potrf".  The naming
conventions and additional documentation for BLAS and LAPACK can be found
at the corresponding website (see below).

---------------------------------------------------------------------------

Up-to-date information (newest release of distribution) can always be
found at:

  http://www.ocaml.info/home/ocaml_sources.html

BLAS and LAPACK (distribution, documentation) can be found at:

   http://www.netlib.org/blas
   http://www.netlib.org/lapack

ATLAS, a very efficient and compatible substitute for BLAS, it specializes
code for specific architectures, can be found here:

  http://www.netlib.org/atlas

Binary packages (e.g. RPMs) for Linux should be available from your
distribution vendor's site.

---------------------------------------------------------------------------

Enjoy!

New York, 2009-04-26
Markus Mottl

email: markus.mottl@gmail.com
www:   http://www.ocaml.info
