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

open Interfaces

module type S = sig
  type t

  include Binable with type binable = t
  include Comparable with type comparable = t
  include Floatable with type floatable = t
  include Hashable.S_binable with type hashable = t
  include Sexpable with type sexpable = t
  include Stringable with type stringable = t
  include Intable with type intable = t

  val to_string_hum : t -> string

  val num_bits : int

  val zero : t
  val one : t
  val minus_one : t

  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  val (/) : t -> t -> t

  

  val neg : t -> t

  val succ : t -> t
  val pred : t -> t

  val abs : t -> t

  (* Integer remainder, with the semantics of mod in Pervasives or rem in Int32/64, i.e.
     if y is not zero, the result of rem x y satisfies the following properties:
     x = (x / y) * y + rem x y and abs(rem x y) <= abs(y)-1.
     If y = 0, rem x y raises Division_by_zero. Notice that rem x y is nonpositive if and
     only if x < 0. *)
  val rem : t -> t -> t

  val max_value : t
  val min_value : t

  val bit_and : t -> t -> t
  val bit_or : t -> t -> t
  val bit_xor : t -> t -> t
  val bit_not : t -> t

  val decr : t ref -> unit
  val incr : t ref -> unit

  
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val shift_right_logical : t -> int -> t

  val of_int32_exn : int32 -> t
  val to_int32_exn : t -> int32
  val of_int64_exn : int64 -> t
  val to_int64 : t -> int64

  val of_nativeint_exn : nativeint -> t
  val to_nativeint_exn : t -> nativeint
end
