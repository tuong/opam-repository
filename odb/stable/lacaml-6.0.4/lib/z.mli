(* File: CZ.mli

   Copyright (C) 2010-

     Christophe Troestler
     email: Christophe.Troestler@umons.ac.be
     WWW: http://math.umons.ac.be/an/

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
(** Complex vectors (precision: complex64). *)

type rvec = (float, float64_elt, fortran_layout) Array1.t
(** Vectors of reals (precision: float64). *)

type mat = (Complex.t, complex64_elt, fortran_layout) Array2.t
(** Complex matrices (precision: complex64). *)

type trans3 = [ `C | `N | `T ]
val prec : (Complex.t, complex64_elt) Bigarray.kind
(** Precision for this submodule {!Z}.  Allows to write precision
    independent code. *)

module Vec : sig
  include module type of Vec2_Z
  include module type of Vec4_Z
end

module Mat : sig
  include module type of Mat2_Z
  include module type of Mat4_Z
end

include module type of Complex_io

include module type of Impl2_Z
include module type of Impl4_Z
