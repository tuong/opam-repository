(******************************************************************************
 *                             Core-extended                                  *
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

open Core.Std

(** Extensions to [Core.Float].*)

val pretty : ?on_negative:[ `Blow_up | `Normal | `Print_dir ]
  -> float
  -> string
(**
   pretty-print a float using no more than five characters, using abberviations
   k, m, g, t.

   if [on_negative] is not set to [`Normal] then the resulting is never over four
   chars but upon negative number we either:
   - raise a failure
   - or print ["<0"]
*)

val to_string_hum : float -> string

(**
   [order_of_magnitude_difference a b]
   by how many orders of magnitude do [a] and [b] differ?
   The return value is non-negative.

   examples:
   - order_of_magnitude_difference   11. 1001.     = 2
   - order_of_magnitude_difference 1001.   11.     = 2
   - order_of_magnitude_difference  131.   11.     = 1
   - order_of_magnitude_difference  200.    0.003  = 5
*)
val order_of_magnitude_difference : float -> float -> int

include Number.Verified_std with type repr = Float.t

module type Fraction = sig
  include S0 with type repr = Float.t
  val one : t
  val random : ?rng : Random.State.t -> unit -> t
end

module Fraction : Fraction with type t = private Float.t
module Fraction_unsafe : Fraction with type t = Float.t
