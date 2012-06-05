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

open Std_internal


module Parts = struct
  type t = {
      sign : Float.Sign.t;
      hr   : int;
      min  : int;
      sec  : int;
      ms   : int;
    }
end

module T : sig
  include Constrained_float.S
  val add     : t -> t -> t
  val sub     : t -> t -> t
  val zero    : t
  val epsilon : t
  val abs     : t -> t
  val scale   : t -> float -> t

  module Constant : sig
    val nanosecond : t
    val microsecond : t
    val millisecond : t
    val second : t
    val minute : t
    val hour : t
    val day : t
  end

  val to_parts : t -> Parts.t
end = struct
  include (Float : sig
    include Constrained_float.S
    val add     : t -> t -> t
    val sub     : t -> t -> t
    val zero    : t
    val epsilon : t
    val abs     : t -> t
    val scale   : t -> float -> t
  end)

  (* this prevents any worry about having these very common names redefined below and
    makes their usage within this module safer.  Constant is included at the very bottom
    to re-export these constants in a more convenient way *)
  module Constant = struct
    (* spans are stored as a float in seconds *)
    let nanosecond  = of_float 1E-9
    let microsecond = of_float 1E-6
    let millisecond = of_float 1E-3
    let second      = of_float 1.
    let minute      = of_float 60.
    let hour        = of_float (60. *. 60.)
    let day         = of_float (24. *. 60. *. 60.)
  end

  let to_parts t =
    let t      = to_float t in
    let sign   = Float.sign t in
    let t      = if Float.(<) t 0. then Float.neg t else t in
    let intval = Float.round_towards_zero_exn t in
    let min    = intval / 60 in
    let sec    = intval mod 60 in
    let hr     = min / 60 in
    let min    = min mod 60 in
    let ms     = Float.iround_exn ((t -. (float intval)) *. 1000.) in
    {Parts.
      sign = sign;
      hr   = hr;
      min  = min;
      sec  = sec;
      ms   = ms;
    }
end

let (/) t f = T.of_float ((t : T.t :> float) /. f)
let (//) (f:T.t) (t:T.t) = (f :> float) /. (t :> float)

let to_ns x        = x // T.Constant.nanosecond
let to_us x        = x // T.Constant.microsecond
let to_ms x        = x // T.Constant.millisecond
let to_sec (x:T.t) = (x :> float)
let to_min x       = x // T.Constant.minute
let to_hr x        = x // T.Constant.hour
let to_day x       = x // T.Constant.day

let ( ** ) f (t:T.t) = T.of_float (f *. (t :> float))
let of_ns x        = x ** T.Constant.nanosecond
let of_us x        = x ** T.Constant.microsecond
let of_ms x        = x ** T.Constant.millisecond
let of_sec x       = T.of_float x
let of_int_sec x   = T.of_float (Float.of_int x)
let of_min x       = x ** T.Constant.minute
let of_hr x        = x ** T.Constant.hour
let of_day x       = x ** T.Constant.day

let randomize (t:T.t) ~percent =
  let t = to_sec t in
  let upperbound = percent *. t in
  let distance = Random.float (2. *. upperbound) -. upperbound in
  of_sec (t +. distance)

let create ?(day = 0) ?(hr = 0) ?(min = 0) ?(sec = 0) ?(ms = 0) () =
  let (+.) = T.add in
  of_day    (Float.of_int day)
  +. of_hr  (Float.of_int hr)
  +. of_min (Float.of_int min)
  +. of_sec (Float.of_int sec)
  +. of_ms  (Float.of_int ms)

include T
include Constant

let of_string (s:string) =
  try
    if String.is_empty s then 
      failwith "empty string"
    else if String.(=) s "inf" || String.(=) s "-inf" then 
      failwith "cannot create infinate span"
    else
      let float n = Float.of_string (String.drop_suffix s n) in
      match s.[String.length s - 1] with
      | 's' ->
          if Int.(=) (String.length s) 1 then failwith "no number given"
          else if Char.(=) s.[String.length s - 2] 'm' then of_ms (float 2)
          else T.of_float (float 1)
      | 'm' -> of_min (float 1)
      | 'h' -> of_hr (float 1)
      | 'd' -> of_day (float 1)
      | _ -> failwith "Time spans must end in ms, s, m, h, or d."
  with exn ->
    invalid_argf "Time.Span.of_string could not parse '%s': %s"
      s (Exn.to_string exn) ()

let of_sexp_error_exn exn sexp =
  of_sexp_error (Exn.to_string exn) sexp

exception T_of_sexp of Sexp.t * exn with sexp
exception T_of_sexp_expected_atom_but_got of Sexp.t with sexp

let t_of_sexp sexp =
  match sexp with
  | Sexp.Atom x ->
    begin
      try of_string x
      with exn -> of_sexp_error_exn (T_of_sexp (sexp, exn)) sexp
    end
  | Sexp.List _ ->
    of_sexp_error_exn (T_of_sexp_expected_atom_but_got sexp) sexp

let to_string (t:T.t) =
  (* this is a sad broken abstraction... *)
  match classify_float (t :> float) with
  | FP_subnormal | FP_zero -> "0s"
  | FP_infinite -> if T.(>) t T.zero then "inf" else "-inf"
  | FP_nan -> "nan"
  | FP_normal ->
      let (<) = T.(<) in
      let abs_t = T.of_float (abs_float (t :> float)) in
      if abs_t < T.Constant.second then sprintf "%gms" (to_ms t)
      else if abs_t < T.Constant.minute then sprintf "%gs" (to_sec t)
      else if abs_t < T.Constant.hour then sprintf "%gm" (to_min t)
      else if abs_t < T.Constant.day then sprintf "%gh" (to_hr t)
      else sprintf "%gd" (to_day t)

let sexp_of_t t = Sexp.Atom (to_string t)

let pp ppf t = Format.fprintf ppf "%s" (to_string t)
let () = Pretty_printer.register "Core.Time.Span.pp"
