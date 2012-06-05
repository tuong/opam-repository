(* File: SD.mli

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

type prec = float32_elt
type num_type = float

type vec = (float, float32_elt, fortran_layout) Array1.t
(** Vectors (precision: float32). *)

type rvec = vec

type mat = (float, float32_elt, fortran_layout) Array2.t
(** Matrices (precision: float32). *)

type trans3 = [ `N | `T ]

val prec : (float, float32_elt) Bigarray.kind
(** Precision for this submodule {!S}.  Allows to write precision
    independent code. *)

module Vec : sig
  include module type of Vec2_S
  include module type of Vec4_S
end

module Mat : sig
  include module type of Mat2_S
  include module type of Mat4_S
end

include module type of Real_io

include module type of Impl2_S
include module type of Impl4_S
