(******************************************************************************
 *                             Core                                           *
 *                                                                            *
 * Copyright (C) 2008- Jane Street Holding, LLC                               *
 *    Contact: opensource@janestreet.com                                      *
 *    WWW: http://www.janestreet.com/ocaml                                    *
 *                                                                            *
 *                                                                            *
 * This library is free software; you can redistribute it and/or              *
 * modify it under the terms of the GNU Lesser General Public                 *
 * License as published by the Free Software Foundation; either               *
 * version 2 of the License, or (at your option) any later version.           *
 *                                                                            *
 * This library is distributed in the hope that it will be useful,            *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 * Lesser General Public License for more details.                            *
 *                                                                            *
 * You should have received a copy of the GNU Lesser General Public           *
 * License along with this library; if not, write to the Free Software        *
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                            *
 ******************************************************************************)

include Int_intf.S with type t = int

val of_int : int -> t
val to_int : t -> int
val of_int32 : int32 -> t option
val to_int32 : t -> int32 option
val of_int64 : int64 -> t option
val of_int64_exn : int64 -> t
val of_nativeint : nativeint -> t option
val to_nativeint : t -> nativeint

module Infix : sig
  (** mod and div operators that have the right behavior on negative numbers,
      that is, [x % y] always returns a positive int between 0 and y-1.
      Invariant: [if r = x % y && q = x /% y then q * y + r = x] *)
  val (%) : t -> t -> t
  val (/%) : t -> t -> t

  (** float division of integers *)
  val (//) : int -> int -> float
end
