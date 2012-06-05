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
open Common

module T = struct
  type t = int with bin_io, sexp

  type binable = t
  type comparable = t
  type floatable = t
  type intable = t
  type sexpable = t
  type stringable = t

  (* if i = j then 0 else if i < j then -1 else 1
     is only slightly faster, so we've decided to stick with
     Pervasives.compare
  *)
  let compare (x : t) y = compare x y
  let equal (x : t) y = x = y
  let hash (x : t) = Hashtbl.hash x

  let of_string s = 
    try
      int_of_string s
    with
    | _ -> failwithf "Int.of_string: %S" s ()

  let to_string = string_of_int
end

include T

let num_bits = Word_size.num_bits Word_size.word_size - 1

let of_float = int_of_float
let to_float = float_of_int

let min (x : t) y = if x < y then x else y
let max (x : t) y = if x > y then x else y
let ascending = compare
let descending x y = compare y x
let equal (x : t) y = x = y
let ( >= ) (x : t) y = x >= y
let ( <= ) (x : t) y = x <= y
let ( = ) (x : t) y = x = y
let ( > ) (x : t) y = x > y
let ( < ) (x : t) y = x < y
let ( <> ) (x : t) y = x <> y

include Hashable.Make_binable (T)
module Map = Core_map.Make (T)
module Set = Core_set.Make (T)

let zero = 0
let one = 1
let minus_one = -1

let pred i = i - 1
let succ i = i + 1

let to_int i = i
let to_int_exn = to_int
let of_int i = i
let of_int_exn = of_int

let max_value = max_int
let min_value = min_int

module Conv = Int_conversions
let of_int32 = Conv.int32_to_int
let of_int32_exn = Conv.int32_to_int_exn
let to_int32 = Conv.int_to_int32
let to_int32_exn = Conv.int_to_int32_exn
let of_int64 = Conv.int64_to_int
let of_int64_exn = Conv.int64_to_int_exn
let to_int64 = Conv.int_to_int64
let of_nativeint = Conv.nativeint_to_int
let of_nativeint_exn = Conv.nativeint_to_int_exn
let to_nativeint = Conv.int_to_nativeint
let to_nativeint_exn = to_nativeint

include Conv.Make (T)

let abs x = abs x

let (+) x y = (+) x y
let (-) x y = (-) x y
let ( * ) x y = ( * ) x y
let (/) x y = (/) x y

module Infix = struct
  let ( % ) x y =
    if y <= 0 then
      invalid_argf
        "%d %% %d in core_int.ml: modulus should be positive" x y ();
    let rval = x mod y in
    if rval < 0
    then rval + y
    else rval

  let ( /% ) x y =
    if y <= 0 then
      invalid_argf
        "%d /%% %d in core_int.ml: divisor should be positive" x y ();
    if x < 0
    then (x + 1) / y - 1
    else x / y

  (** float division of integers *)
  let (//) x y = float x /. float y
end

let neg x = -x


let rem a b = a mod b
let incr = Pervasives.incr
let decr = Pervasives.decr

let shift_right a b = a asr b
let shift_right_logical a b = a lsr b
let shift_left a b = a lsl b
let bit_not a = lnot a
let bit_or a b = a lor b
let bit_and a b = a land b
let bit_xor a b = a lxor b
