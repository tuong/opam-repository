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
open Time_internal.Helpers

(**
 * XXX disgusting hack! XXX
 * round to some arbitrary level of precision because when we print we're going
 * to floor eventually.
 *  So e.g. 6.045 might turn into * 0.0449999999999aosneuthsaoenuth
 * after a modf--this will then get printed as 6.044 which is DAMN WRONG.
 **)

let canonicalize x =
  let canonical_multiplier = 1_000_000. in
  Float.round (x *. canonical_multiplier) /. canonical_multiplier

(* Create an abstract type for Ofday to prevent us from confusing it with
    other floats.
*)
module T : sig
  include Constrained_float.S
  val add : t -> Span.t -> t option
  val sub : t -> Span.t -> t option
  val diff : t -> t -> Span.t
  val of_sec : float -> t
  val to_sec : t -> float
  val of_span_since_midnight : Span.t -> t
  val to_span_since_midnight : t -> Span.t
  val start_of_day : t
  val end_of_day : t
end = struct
  (* Number of seconds since midnight. *)
  
  include Float

  let to_sec = ident

  
  let of_sec s =
    match classify_float s with
    | FP_infinite -> invalid_arg "Time.Ofday.of_sec: infinite value"
    | FP_nan -> invalid_arg "Time.Ofday.of_sec: NaN value"
    | FP_normal | FP_subnormal | FP_zero ->
        let span = Span.of_sec s in
        if Span.(>=) span Span.day || Span.(<) span Span.zero
        then invalid_argf "Time.Ofday.of_sec: out of range: %f" s ()
        else canonicalize s

  let to_span_since_midnight t = Span.of_sec t
  let of_span_since_midnight span = of_sec (Span.to_sec span)

  let start_of_day = 0.
  let end_of_day = of_span_since_midnight (Span.sub Span.day Span.epsilon)

  let is_valid (t:t) =
    let t = to_span_since_midnight t in
    Span.(<=) Span.zero t && Span.(<) t Span.day

  let add (t:t) (span:Span.t) =
    let t = t +. (Span.to_sec span) in
    if is_valid t then Some t else None
  ;;

  let sub (t:t) (span:Span.t) =
    let t = t -. (Span.to_sec span) in
    if is_valid t then Some t else None
  ;;

  let diff t1 t2 = Span.sub (to_span_since_midnight t1) (to_span_since_midnight t2)
end

let create ?hr ?min ?sec ?ms () =
  T.of_span_since_midnight (Span.create ?hr ?min ?sec ?ms ())
;;

let to_parts t = Span.to_parts (T.to_span_since_midnight t)

let of_sec = T.of_sec


let to_string_gen ~drop_ms ~drop_us ~trim x =
  assert (if drop_ms then drop_us else true);
  let buf = Buffer.create 16 in
  let x = T.to_sec x in
  
  if x = neg_infinity then "mintime"
  else if x = infinity then "maxtime"
  else
    (* tot_us can be at most 24*60*60*1_000_000 = 86_400_000_000, which is why we
        need to use 63 bits integers *)
    let tot_us = Core_int63.of_float (x *. 1_000_000. +. 1.E-4) in
    let one_thousand = Core_int63.of_int 1_000 in
    let int63_to_int x =
      match Core_int63.to_int x with
      | Some x -> x
      | None ->
        raise (Bug (sprintf "Time.int63_to_int: tried to convert %s"
                        (Core_int63.to_string x)))
    in
    (* at most 1_000 *)
    let us = int63_to_int (Core_int63.rem tot_us one_thousand) in
    (* at most 24*60*60*1000 = 86_400_000, i.e. it fits even in 32 bits *)
    let tot_ms = int63_to_int (Core_int63.(/) tot_us one_thousand) in
    let ms = tot_ms mod 1_000 in
    let tot_s = tot_ms / 1_000 in
    let s = tot_s mod 60 in
    let tot_m = tot_s / 60 in
    let m = tot_m mod 60 in
    let h = tot_m / 60 in
    Buffer.add_string buf (string_of_int_2_digits h);
    Buffer.add_char buf ':';
    Buffer.add_string buf (string_of_int_2_digits m);
    let dont_print_us = (trim && us = 0) || drop_us in
    let dont_print_ms = (trim && ms = 0 && dont_print_us) || drop_ms in
    let dont_print_s = trim && s = 0 && dont_print_ms in
    if dont_print_s then ()
    else begin
      Buffer.add_char buf ':';
      Buffer.add_string buf (string_of_int_2_digits s);
      if dont_print_ms then ()
      else begin
        Buffer.add_char buf '.';
        Buffer.add_string buf (string_of_int_3_digits ms);
        if dont_print_us then ()
        else Buffer.add_string buf (string_of_int_3_digits us)
      end
    end;
    Buffer.contents buf
;;

let to_string t = to_string_gen ~drop_ms:false ~drop_us:false ~trim:false t

let to_string_trimmed t = to_string_gen ~drop_ms:false ~drop_us:false ~trim:true t

let to_sec_string t = to_string_gen ~drop_ms:true ~drop_us:true ~trim:false t

let to_millisec_string t = to_string_gen ~drop_ms:false ~drop_us:true ~trim:false t


let of_string_iso8601_extended ?pos ?len str =
  let (pos, len) =
    match (Ordered_collection_common.get_pos_len ?pos ?len
              ~length:(String.length str))
    with
    | Result.Ok z -> z
    | Result.Error s ->
        failwithf "Time.Ofday.of_string_iso8601_extended: %s" s ()
  in
  try
    if len < 2 then failwith "len < 2"
    else
      T.of_span_since_midnight
        (
          let hour = parse_two_digits str pos in
          if hour > 24 then failwith "hour > 24";
          let span = Span.of_hr (float hour) in
          if len = 2 then span
          else if len < 5 then failwith "2 < len < 5"
          else if str.[pos + 2] <> ':' then failwith "first colon missing"
          else
            let minute = parse_two_digits str (pos + 3) in
            if minute >= 60 then failwith "minute > 60";
            let span = Span.add span (Span.of_min (float minute)) in
            if hour = 24 && minute <> 0 then
              failwith "24 hours and non-zero minute";
            if len = 5 then span
            else if len < 8 then failwith "5 < len < 8"
            else if str.[pos + 5] <> ':' then failwith "second colon missing"
            else
              let second = parse_two_digits str (pos + 6) in
              if second >= 60 then failwith "second > 60";
              let span = Span.add span (Span.of_sec (float second)) in
              if hour = 24 && second <> 0 then
                failwith "24 hours and non-zero seconds";
              if len = 8 then span
              else if len = 9 then failwith "length = 9"
              else
                match str.[pos + 8] with
                | '.' | ',' ->
                    let last = pos + len - 1 in
                    let rec loop pos subs =
                      let subs = subs * 10 + Char.get_digit_exn str.[pos] in
                      if pos = last then subs else loop (pos + 1) subs
                    in
                    let subs = loop (pos + 9) 0 in
                    if hour = 24 && subs <> 0 then
                      failwith "24 hours and non-zero subseconds"
                    else
                      Span.add span
                        (Span.of_sec (float subs /. (10. ** float (len - 9))))
                | _ -> failwith "missing subsecond separator"
        )
  with exn ->
    invalid_argf "Time.Ofday.of_string_iso8601_extended(%s): %s"
      (String.sub str ~pos ~len) (Exn.to_string exn) ()
;;

let of_int_sec s = of_sec (float s)
let of_int_ms ms = of_sec ((float ms) /. 1000.)
let of_sec_ms sec ms = of_sec ((float sec) +. (float ms) /. 1000.)

let hour = 3600.
let small_diff ofday1 ofday2 =
  let ofday1 = T.to_sec ofday1 in
  let ofday2 = T.to_sec ofday2 in
  let diff   = ofday1 -. ofday2 in
  (* if one of [ofdayN] is either [(neg_)infinity] or [nan], so is [diff] *)
  match classify_float diff with
  | FP_infinite | FP_nan -> None
  | _ ->
      (*  d1 is in (-hour; hour) *)
      let d1 = mod_float diff hour in
      (*  d2 is in (0;hour) *)
      let d2 = mod_float (d1 +. hour) hour in
      let d = if d2 > hour /. 2. then d2 -. hour else d2 in
      Some (Span.of_sec d)

let pp ppf t = Format.fprintf ppf "%s" (to_string t)
let () = Pretty_printer.register "Core.Time.Ofday.pp"

(* There are a number of things that would be shadowed by this include because of the
   scope of Constrained_float.  These need to be defined below.  It's a an unfortunate
   situation because we would like to say include T, without shadowing. *)
include T

let to_string t = to_string_gen ~drop_ms:false ~drop_us:false ~trim:false t

let of_string s =
  try
    let create h m s =
      of_sec
        (float (Int.of_string h * 60 * 60 + Int.of_string m * 60) +. s)
    in
    match String.split s ~on:':' with
    | [h; m; s] -> create h m (Float.of_string s)
    | [h; m] -> create h m 0.
    | [hm] ->
        if Pervasives.(=) (String.length hm) 4 then
          create
            (String.sub hm ~pos:0 ~len:2) (String.sub hm ~pos:2 ~len:2) 0.
        else failwith "No colon, expected string of length four"
    | _ -> failwith "More than two colons"
  with exn ->
    invalid_argf "Time.Ofday.of_string (%s): %s" s (Exn.to_string exn) ()
;;

let t_of_sexp sexp =
  match sexp with
  | Sexp.Atom s ->
      (try of_string s
      with Invalid_argument s ->
        of_sexp_error ("Time.Ofday.t_of_sexp: " ^ s) sexp)
  | _ -> of_sexp_error "Time.Ofday.t_of_sexp" sexp

let sexp_of_t span = Sexp.Atom (to_string span)


