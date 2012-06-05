(* File: complex64.ml

   Copyright (C) 2005-

     Markus Mottl
     email: markus.mottl@gmail.com
     WWW: http://www.ocaml.info

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Bigarray

type prec = complex64_elt
type num_type = Complex.t
type vec = (Complex.t, complex64_elt, fortran_layout) Array1.t
type rvec = (float, float64_elt, fortran_layout) Array1.t
type mat = (Complex.t, complex64_elt, fortran_layout) Array2.t

type trans3 = [ `N | `T | `C ]

let prec = complex64
let zero = Complex.zero
let one = Complex.one
let add = Complex.add

let int_of_complex64 z = int_of_float z.Complex.re
