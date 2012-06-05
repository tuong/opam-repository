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

open Common
open Std_internal
module Unix = Core_unix

include Time_internal

let of_unix mktime seconds_in_day date =
  let parts = Float.modf seconds_in_day in
  let float_part, sec = Float.Parts.fractional parts, Float.Parts.integral parts in
  let sec = Int.of_float sec in
  let uday =
    { Unix.
      tm_sec = sec mod 60;
      tm_min = (sec / 60) mod 60;
      tm_hour = sec / 3600;
      tm_mday = date.Date.d;
      tm_mon = Month.to_int date.Date.m - 1;
      tm_year = date.Date.y - 1900;
      tm_wday = 0;
      tm_yday = 0;
      tm_isdst = false;
    }
  in
  let time =
    try mktime uday
    with Unix.Unix_error (e,s1,s2) ->
      failwithf "Unix error converting date: (%s,%s,%s)"
        (Unix.error_message e) s1 s2 ()
  in
  T.of_float (time +. float_part)

let of_date_ofday_impl ~suffix mktime date ofday =
  try
    let ofday = Span.of_sec (Ofday.to_sec ofday) in
    
    if Span.(>) ofday Span.day then
      failwithf "%g seconds > 24 hours" (Span.to_sec ofday) ();
    if Span.(<) ofday Span.zero then failwithf "%g seconds < 0." (Span.to_sec ofday) ();
    of_unix mktime (Span.to_sec ofday) date
  with exn ->
    invalid_argf "Time.of_date_ofday%s %s %s: %s"
      suffix (Date.to_string date) (Ofday.to_string ofday) (Exn.to_string exn) ()

let of_date_ofday date ofday =
  of_date_ofday_impl ~suffix:"" (fun uday -> fst (Unix.mktime uday)) date ofday

let of_date_ofday_utc date ofday =
  of_date_ofday_impl ~suffix:"_utc" Unix.timegm date ofday

let tm_subs_to_ofday tm subs =
  subs +. float (tm.Unix.tm_sec + 60 * (tm.Unix.tm_min + tm.Unix.tm_hour * 60))

let to_date_ofday_impl tmfun time =
  try
    let time = T.to_float time in
    let parts = Float.modf time in
    let subs, sec = Float.Parts.fractional parts, Float.Parts.integral parts in
    let tm = tmfun sec in
    let ofday = tm_subs_to_ofday tm subs in
    Date.of_tm tm, Ofday.of_sec ofday
  with
  | e -> 
    (* We can guarantee that a time can become a float.  Any other conversion (say, to a
        string) might fail in the same way as the date/ofday conversion *)
    failwithf "unable to convert %f to date/ofday: %s" 
      (T.to_float time) (Exn.to_string e) ()

let to_date_ofday time = to_date_ofday_impl Unix.localtime time
let to_date_ofday_utc time = to_date_ofday_impl Unix.gmtime time

let to_ofday t = snd (to_date_ofday t)

let of_string_abs string =
  let raise_invalid =
    invalid_argf "Time.of_string_abs : %s" string
  in
  let (date, time) =
    match String.lsplit2 ~on:' ' string with
    | Some date_time -> date_time
    | None ->
        match String.lsplit2 ~on:'T' string with
        | Some date_time -> date_time
        | None -> raise_invalid ()
  in
  let tof = Float.of_string in
  let toi = Int.of_string in
  let seconds_in_day, mktime =
    match String.split ~on:':' time with
    | hh :: mm :: ss :: offset ->
      let hh = toi hh in
      let mm = toi mm in
      let ss, mktime, offset =
        (* The offset is included in the seconds *)
        match offset with
        | [hh; mm] ->
          (tof ss), Unix.timegm, float_of_hh_mm_ss (toi hh) (toi mm) 0.
        | [] ->
          (match String.split ~on:'Z' ss with
          | [ss; ""] ->
            (tof ss), Unix.timegm, 0.
          | [ss] ->
            (tof ss), (fun uday -> fst (Unix.mktime uday)), 0.
          | _ -> raise_invalid ())
        | _ -> raise_invalid ()
      in
      (float_of_hh_mm_ss hh mm ss) -. offset, mktime
    | _ -> raise_invalid ()
  in
  let date = Date.of_string date in
  let date, seconds_in_day =
    if seconds_in_day < 0. then
      Date.add_days date ~-1, ((Span.to_sec Span.day) +. seconds_in_day)
    else if seconds_in_day > Span.to_sec Span.day then
      Date.add_days date 1, seconds_in_day -. (Span.to_sec Span.day)
    else date, seconds_in_day
  in
  if seconds_in_day > (Span.to_sec Span.day) then raise_invalid ()
  else of_unix mktime seconds_in_day date

let to_string_abs time =
  let date, sec = to_date_ofday_utc time in
  (Date.to_string date) ^ "T" ^ (Ofday.to_string sec) ^ "Z"

let to_string_trimmed t =
  let date, sec = to_date_ofday t in
  (Date.to_string date) ^ " " ^ (Ofday.to_string_trimmed sec)

let to_sec_string t =
  let date, sec = to_date_ofday t in
  (Date.to_string date) ^ " " ^ (Ofday.to_sec_string sec)

  let to_string_old t =
  let date, sec = to_date_ofday t in
  (Date.to_string_old date) ^ " " ^ (Ofday.to_string sec)

let to_filename_string t =
  let date, ofday = to_date_ofday t in
  (Date.to_string date) ^ "_" ^
    (String.tr ~target:':' ~replacement:'-' (Ofday.to_string ofday))

let to_string_fix_proto utc t =
  let date, sec =
    match utc with
    | `Utc -> to_date_ofday_utc t
    | `Local -> to_date_ofday t
  in
  (Date.to_string_iso8601_basic date) ^ "-" ^ (Ofday.to_millisec_string sec)

let of_string_fix_proto utc str =
  try
    let expect_length = 21 in  (* = 8 + 1 + 12 *)
    let expect_dash = 8 in
    if str.[expect_dash] <> '-' then
        failwithf "no dash in position %d" expect_dash ();
    let of_date_ofday =
      match utc with
      | `Utc -> of_date_ofday_utc
      | `Local -> of_date_ofday
    in
    if Int.(>) (String.length str) expect_length then
      failwithf "input too long" ();
    of_date_ofday
      (Date.of_string_iso8601_basic str ~pos:0)
      (Ofday.of_string_iso8601_extended str ~pos:(expect_dash + 1))
  with exn ->
    invalid_argf "Time.of_string_fix_proto %s: %s" str (Exn.to_string exn) ()

let of_filename_string s =
  try
    match String.lsplit2 s ~on:'_' with
    | None -> failwith "no space in filename string"
    | Some (date, ofday) ->
        let date = Date.of_string date in
        let ofday = String.tr ~target:'-' ~replacement:':' ofday in
        let ofday = Ofday.of_string ofday in
        of_date_ofday date ofday
  with
  | exn ->
      invalid_argf "Time.of_filename_string (%s): %s" s (Exn.to_string exn) ()

let of_date_time_strings date_string time_string =
  of_date_ofday (Date.of_string date_string) (Ofday.of_string time_string)

let of_date_time_strings_utc date_string time_string =
  of_date_ofday_utc (Date.of_string date_string) (Ofday.of_string time_string)

let format t s = Unix.strftime (to_tm t) s

let to_date = Date.of_time

(** Pause (and don't throw an exception)  *)
let pause span =
  (* If too large a float is passed in (Span.max_value for instance) then
      select will return immediately, leading to an infinite and expensive
      select loop.  This is handled below by pausing for no longer than 100 days
      at a time. *)
  let span = Span.min span (Span.scale Span.day 100.) in
  let finish = T.add (T.now ()) span in
  let rec pause_for span =
    begin
      try
        ignore
          (Unix.select
              ~read:[]
              ~write:[]
              ~except:[]
              ~timeout:(Span.to_sec span) ())
      with
      | Sys.Break -> raise Sys.Break
      | _ -> ()
    end;
    let now = T.now () in
    if T.(>) finish now
    then pause_for (T.diff finish now)
  in
  pause_for span

let rec pause_forever () =
  pause (Span.of_day 1.0);
  pause_forever ()

let ofday_occurrence_gen ~utc = ();
  fun ofday before_or_after time ->
    let first_guess =
      if utc then
        of_date_ofday_utc (fst (to_date_ofday_utc time)) ofday
      else
        of_date_ofday (to_date time) ofday
    in
    match before_or_after with
    | `right_before ->
        if T.(<) first_guess time
        then first_guess
        else T.sub first_guess Span.day
    | `right_after ->
        if T.(>) first_guess time
        then first_guess
        else T.add first_guess Span.day

let ofday_occurrence = ofday_occurrence_gen ~utc:false
let ofday_occurrence_utc = ofday_occurrence_gen ~utc:true

let to_date = Date.of_time

let epoch = T.of_float 0.0


module UTC = struct
  type t = T.t

  type stringable = t
  type sexpable = t

  let of_string s =
    try
      (* use rsplit2 instead of lsplit2 because date can contain a space,
         e.g., "01 JAN 2008 10:37:22.551", while ofday cannot *)
      match String.rsplit2 s ~on:' ' with
      | None -> invalid_arg (sprintf "no space in Time.of_string: %s" s)
      | Some (date,time) -> of_date_time_strings_utc date time
    with
    | e -> invalid_arg (sprintf "Time.of_string_utc: %s" (Exn.to_string e))

  let t_of_sexp sexp = match sexp with
    | Sexp.List [Sexp.Atom date; Sexp.Atom ofday] ->
        begin
          try of_string (date ^ " " ^ ofday)
          with
        e -> of_sexp_error (sprintf "Time.t_of_sexp: %s" (Exn.to_string e)) sexp
        end
    | _ -> of_sexp_error "Time.t_of_sexp" sexp

  let to_string t =
    let date, sec = to_date_ofday_utc t in
    sprintf "%s %s" (Date.to_string date) (Ofday.to_string sec)

  let sexp_of_t t =
    match String.lsplit2 (to_string t) ~on:' ' with
    | Some (date,ofday) ->
        Sexp.List [Sexp.Atom date; Sexp.Atom ofday]
    | None ->
        raise (Bug "Time.sexp_of_utc: unexpected None")
end

(* There are a number of things that would be shadowed by this include because of the
   scope of Constrained_float.  These need to be defined below.  It's a an unfortunate
   situation because we would like to say include T, without shadowing. *)
include T

let to_string t =
  let date, sec = to_date_ofday t in
  (* for small strings ^ is about 5 times faster than sprintf, and
      even slightly faster than using a buffer. *)
  (Date.to_string date) ^ " " ^ (Ofday.to_string sec)

let of_string s =
  try
    (* use rsplit2 instead of lsplit2 because date can contain a space,
        e.g., "01 JAN 2008 10:37:22.551", while ofday cannot *)
    match String.rsplit2 s ~on:' ' with
    | Some (date,time) -> of_date_time_strings date time
    | None ->
        (* The standard XML date format is:
            YYYY-MM-DDTHH:MM:SS.SS *)
        match String.rsplit2 s ~on:'T' with
        | Some (date, time) -> of_date_time_strings date time
        | None -> invalid_arg (sprintf "no space or T in Time.of_string: %s" s)
  with
  | e -> invalid_arg (sprintf "Time.of_string: %s" (Exn.to_string e))

let t_of_sexp sexp = match sexp with
  | Sexp.List [Sexp.Atom date; Sexp.Atom ofday] ->
      begin
        try of_string (date ^ " " ^ ofday)
        with
      e -> of_sexp_error
        (sprintf "Time.t_of_sexp (2 atoms): %s" (Exn.to_string e)) sexp
      end
  | Sexp.Atom datetime ->
      begin
        try of_string datetime with
        e -> of_sexp_error (sprintf "Time.t_of_sexp: %s" (Exn.to_string e)) sexp
      end
  | _ -> of_sexp_error "Time.t_of_sexp" sexp

let sexp_of_t t =
  match String.lsplit2 (to_string t) ~on:' ' with
  | Some (date,ofday) ->
      Sexp.List [Sexp.Atom date; Sexp.Atom ofday]
  | None ->
      raise (Bug "Time.sexp_of_t: unexpected None")

let pp ppf t = Format.fprintf ppf "%s" (to_string t)
let () = Pretty_printer.register "Core.Time.pp"


