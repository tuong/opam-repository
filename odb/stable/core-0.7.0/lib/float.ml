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
module Sexp = Sexplib.Sexp
module String = Core_string
open Core_printf

module T = struct
  type t = float with sexp, bin_io

  type binable = t
  type sexpable = t

  let compare (x : t) y = compare x y
  let equal (x : t) y = x = y
  let hash (x : t) = Hashtbl.hash_param 1 1 x
end

include T
type outer = t with bin_io,sexp (* alias for use by sub-modules *)

type floatable = t
let to_float x = x
let of_float x = x

type stringable = t


let of_string s =
  try float_of_string s with
  | _ -> invalid_argf "Float.of_string %s" s ()
;;

let to_string = string_of_float

let max_value = infinity
let min_value = neg_infinity
let max_finite_value = Pervasives.max_float
let min_finite_value = Pervasives.min_float
let zero = 0.

let is_nan x = (x : t) <> x
include Float_robust_compare


include Hashable.Make_binable (T)

let of_int = Pervasives.float_of_int

let to_int f =
  match classify_float f with
  | FP_normal | FP_subnormal | FP_zero -> int_of_float f
  | FP_infinite | FP_nan -> invalid_arg "Float.to_int on nan or inf"

let of_int64 i = Int64.to_float i

let to_int64 f =
  match classify_float f with
  | FP_normal | FP_subnormal | FP_zero -> Int64.of_float f
  | FP_infinite | FP_nan -> invalid_arg "Float.to_int64 on nan or inf"

(* max_int/min_int are architecture dependent, e.g. +/- 2^30, +/- 2^62 if 32-bit, 64-bit
   (respectively) while float is IEEE standard for double (52 significant bits).  We may
   lose precision (e.g. beyond the 52 bits) as we round from float to int but, by capping
   with float_round_lb and float_round_ub, we shouldn't run afoul of flipping the sign bit
   on our integers.  With strict inequalities used on float_round_lb and float_round_ub we
   could actually define them without the +. or -. 257.; this is a bit of extra precaution
   in case someone uses them with <= or >=.
*)
let float_round_lb = max (float_of_int min_int) (-1.0 *. 2.0 ** 62.0 +. 257.)
let float_round_ub = min (float_of_int max_int) (2.0 ** 62.0 -. 257.)
let int_round_lb = int_of_float float_round_lb
let int_round_ub = int_of_float float_round_ub

let round_towards_zero_exn x =
  if is_nan x then
    invalid_arg "Float.round_towards_zero_exn: Unable to handle NaN"
  else
    begin
      if float_round_lb < x && x < float_round_ub then truncate x
      else invalid_argf "Float.round_towards_zero_exn: argument out of bounds (%f)" x ()
    end

let round_towards_zero x =
  try Some (round_towards_zero_exn x)
  with _ -> None

let round x = floor (x +. 0.5)

let round_down_exn x =
  if is_nan x then
    invalid_arg "Float.round_down_exn: Unable to handle NaN"
  else
    begin
      if float_round_lb < x && x < float_round_ub then int_of_float (floor x)
      else invalid_argf "Float.round_down_exn: argument out of bounds (%f)" x ()
    end

let round_down x =
  try Some (round_down_exn x)
  with _ -> None

let round_up_exn x =
  if is_nan x then
    invalid_arg "Float.round_up_exn: Unable to handle NaN"
  else
    begin
      if float_round_lb < x && x < float_round_ub then int_of_float (ceil x)
      else invalid_argf "Float.round_up_exn: argument out of bounds (%f)" x ()
    end

let round_up x =
  try Some (round_up_exn x)
  with _ -> None

let iround x =
  if float_round_lb < x && x < float_round_ub then
    Some (int_of_float (round x))
  else None (* float too big to round reliably to int *)

let iround_exn x =
  match iround x with
  | None -> invalid_argf "Float.iround_exn: argument out of bounds (%f)" x ()
  | Some n -> n

let is_inf x = (classify_float x = FP_infinite);;

let min_inan (x : t) y =
  if is_nan y then x
  else if is_nan x then y
  else if x < y then x else y

let max_inan (x : t) y =
  if is_nan y then x
  else if is_nan x then y
  else if x > y then x else y

let add = (+.)
let sub = (-.)
let neg = (~-.)
let abs = abs_float
let scale = ( *. )

type comparable = t
let min (x : t) y =
  if is_nan x || is_nan y then nan
  else if x < y then x else y
let max (x : t) y =
  if is_nan x || is_nan y then nan
  else if x > y then x else y

module Parts = struct
  type t =
    { fractional : float;
      integral   : float;
    }

  let fractional t = t.fractional
  let integral t = t.integral
end

let modf t =
  let (fractional, integral) = modf t in
  { Parts.
    fractional = fractional;
    integral = integral; }
let floor = floor
let ceil = ceil
let mod_float = mod_float

module Class = struct
  type t =
  | Infinite
  | Nan
  | Normal
  | Subnormal
  | Zero
  with sexp, bin_io

  type binable = t
  type sexpable = t
  type stringable = t

  let to_string t = Sexp.to_string (sexp_of_t t)
  let of_string s = t_of_sexp (Sexp.Atom s)
end

let classify t =
  let module C = Class in
  match Pervasives.classify_float t with
  | FP_normal -> C.Normal
  | FP_subnormal -> C.Subnormal
  | FP_zero -> C.Zero
  | FP_infinite -> C.Infinite
  | FP_nan -> C.Nan
;;

let compare (x : t) y = compare x y
let ascending = compare
let descending x y = compare y x

let ( >= ) (x : t) y = x >= y
let ( <= ) (x : t) y = x <= y
let ( = ) (x : t) y = x = y
let ( > ) (x : t) y = x > y
let ( < ) (x : t) y = x < y
let ( <> ) (x : t) y = x <> y

let (+) t t' = t +. t'
let (-) t t' = t -. t'
let ( * ) t t' = t *. t'
let (/) t t' = t /. t'

module Set = Core_set.Make_binable (T)
module Map = Core_map.Make_binable (T)

module Sign = struct
  type t = Neg | Zero | Pos
end

let sign t =
  if t >. 0. then Sign.Pos
  else if t <. 0. then Sign.Neg
  else Sign.Zero

module Terse = struct
  type t = outer with bin_io
  let t_of_sexp = t_of_sexp

  type binable = t
  type sexpable = t
  type stringable = t

  let to_string x = Core_printf.sprintf "%.8G" x
  let sexp_of_t x = Sexp.Atom (to_string x)
  let of_string x = of_string x
end
