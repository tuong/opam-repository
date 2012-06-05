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

open Sexplib.Wrapper
open Bin_prot.Std

open Nativeint

module T = struct
  type t = nativeint with sexp, bin_io

  type binable = t
  type floatable = t
  type intable = t
  type sexpable = t
  type stringable = t

  let compare (x : t) y = compare x y
  let equal (x : t) y = x = y
  let hash (x : t) = Hashtbl.hash x

  let to_string = to_string
  let of_string = of_string
end

include T

let num_bits = Word_size.num_bits Word_size.word_size

let shift_right_logical = shift_right_logical
let shift_right = shift_right
let shift_left = shift_left
let bit_not = lognot
let bit_xor = logxor
let bit_or = logor
let bit_and = logand
let min_value = min_int
let max_value = max_int
let abs = abs
let pred = pred
let succ = succ
let rem = rem
let neg = neg
let minus_one = minus_one
let one = one
let zero = zero
let to_float = to_float
let of_float = of_float

type comparable = t
let ascending = compare
let descending x y = compare y x
let min (x : t) y = if x < y then x else y
let max (x : t) y = if x > y then x else y
let ( >= ) (x : t) y = x >= y
let ( <= ) (x : t) y = x <= y
let ( = ) (x : t) y = x = y
let ( > ) (x : t) y = x > y
let ( < ) (x : t) y = x < y
let ( <> ) (x : t) y = x <> y

include Hashable.Make_binable (T)
module Map = Core_map.Make (T)
module Set = Core_set.Make (T)

let ( / ) = div
let ( * ) = mul
let ( - ) = sub
let ( + ) = add

let incr r = r := !r + one
let decr r = r := !r - one

let of_nativeint t = t
let of_nativeint_exn = of_nativeint
let to_nativeint t = t
let to_nativeint_exn = to_nativeint

module Conv = Int_conversions
let of_int = Conv.int_to_nativeint
let of_int_exn = of_int
let to_int = Conv.nativeint_to_int
let to_int_exn = Conv.nativeint_to_int_exn
let of_int32 = Conv.int32_to_nativeint
let of_int32_exn = of_int32
let to_int32 = Conv.nativeint_to_int32
let to_int32_exn = Conv.nativeint_to_int32_exn
let of_int64 = Conv.int64_to_nativeint
let of_int64_exn = Conv.int64_to_nativeint_exn
let to_int64 = Conv.nativeint_to_int64

include Conv.Make (T)
