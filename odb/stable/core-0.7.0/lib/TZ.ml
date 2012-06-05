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
module Hashtbl = Core_hashtbl
module Unix = Core_unix

exception Unknown_zone of string with sexp
exception Invalid_file_format of string with sexp

type epoch_type = Localtime | UTC

module Digest = struct
  include Digest

  include Bin_prot.Utils.Make_binable (struct
    module Binable = struct
      type t = string with bin_io
      type binable = t
    end

    let to_binable str = str
    let of_binable str = str

    type t = string
  end)

  let sexp_of_t t = Sexp.Atom (Digest.to_hex t)
end

(* this is a recreation of the algorithm used internally by the linux kernel
  (supposedly invented by Gauss).  In this case it is used to produce the number
  of seconds since 1970-01-01 00:00:00 using epoch time semantics (86,400 seconds
  per day *)
let utc_mktime ~year ~month ~day ~hour ~min ~sec ~msec =
  (* move February to the conceptual end of the ordering - 1..12 -> 11,12,1..10 -
    because it carries the leap day.  The months are 0 indexed for this calculation,
    so 1 is February. *)
  let shuffle_year_month year month =
    let month = month - 2 in
    if month <= 0 then (year - 1, month + 12) else (year,month)
  in
  let hour       = float_of_int hour in
  let min        = float_of_int min in
  let sec        = float_of_int sec in
  let msec       = float_of_int msec in
  let year,month = shuffle_year_month year month in
  let days       = year / 4 - year / 100 + year / 400 + 367 * month / 12 + day in
  let days       = float_of_int days +. 365. *. float_of_int year -. 719499. in
  let hours      = 24. *. days +. hour in
  let mins       = 60. *. hours +. min in
  60. *. mins +. sec +. msec /. 1000.

(*
  A time zone file is made up (conceptually - the reality is more compact) of an ordered
  list of (float * [local_time_type]) that marks the boundaries (marked from the epoch)
  at which various time adjustment regimes are in effect.  This can also be thought of
  as breaking down all time past the epoch into ranges with a [local_time_type] that
  describes the offset from GMT to apply to each range to get local time.
*)
module Local_time_type = struct
  type t = {
    gmt_off: float;
    is_dst: bool;
    abbrv: string;
  } with sexp_of, bin_io
end

(* holds information about when leap seconds should be applied - unused
  because we are translating based on a epoch system clock (see the .mli). *)
module Leap_second = struct
  type t = {
    time: float;
    seconds: float;
  } with sexp_of, bin_io
end

module Zone_base = struct
  type t = {
    name: string;
    file_digest: Digest.t;
    transitions: (float * Local_time_type.t) array;
    default_local_time_type: Local_time_type.t;
    leap_seconds: Leap_second.t list;
  } with sexp_of, bin_io
end

(* [find_local_time_type seconds zone UTC] finds the local time type
   of [zone] at [seconds], from 1970/01/01:00:00:00 GMT.

   [find_local_time_type seconds zone Localtime] finds the local time type of
   [zone] at [seconds], from 1970/01/01:00:00:00 of [zone].
*)
let find_local_time_type time zone transtype =
  let transition_as_utc (transition_utc, _lt) = transition_utc in
  let transition_as_localtime (transition_utc, lt) =
    transition_utc +. lt.Local_time_type.gmt_off
  in
  let convert_transition =
    match transtype with
    | Localtime -> transition_as_localtime
    | UTC -> transition_as_utc
  in
  let num_transitions = Array.length zone.Zone_base.transitions in
  let index_opt =
    Array.findi zone.Zone_base.transitions ~f:(fun transition ->
      convert_transition transition > time)
  in
  match index_opt with
  | Some 0 ->
      zone.Zone_base.default_local_time_type
  | Some i ->
      snd zone.Zone_base.transitions.(i - 1)
  | None ->
      if num_transitions = 0 then zone.Zone_base.default_local_time_type
      else snd zone.Zone_base.transitions.(num_transitions - 1)

let shift_epoch_time repr_type epoch zone =
  match repr_type with
  | Localtime ->
      let lt = find_local_time_type epoch zone Localtime in
      epoch -. lt.Local_time_type.gmt_off
  | UTC ->
      let lt = find_local_time_type epoch zone UTC in
      epoch +. lt.Local_time_type.gmt_off

module Date_time = struct
  
  type t = Date.t * Ofday.t

  let of_epoch time (* shifted epoch for the time zone for conversion *) =
    let subsec    = time -. floor time in
    let tm        = Unix.gmtime time in
    let date      = Date.of_tm tm in
    let hr        = float_of_int tm.Unix.tm_hour in
    let min       = float_of_int tm.Unix.tm_min in
    let sec       = float_of_int tm.Unix.tm_sec in
    let total_sec = 60. *. 60. *. hr +. 60. *. min +. sec +. subsec in
    let ofday     = Ofday.of_sec total_sec in
    (date,ofday)

  let of_time time zone =
    try
      of_epoch (shift_epoch_time UTC (Time.to_float time) zone)
    with
    | Unix.Unix_error(_, "gmtime", _) -> raise (Invalid_argument "TZ.Datetime_of_time")

  let to_time (date, ofday) zone =
    let time =
      utc_mktime ~year:date.Date.y ~month:(Month.to_int date.Date.m)
        ~day:date.Date.d ~hour:0 ~min:0 ~sec:0 ~msec:0 in
    let time = time +. Ofday.to_float ofday in
    Time.of_float (shift_epoch_time Localtime time zone)

  let convert ~from_tz ~to_tz time =
    let start_time = Time.to_float (to_time time from_tz) in
    of_epoch (shift_epoch_time UTC start_time to_tz)
end

module Db = struct
  type t = {
    mutable full: bool;
    basedir: string;
    table: Zone_base.t String.Table.t
  }

  let bool_of_int i = i <> 0

  let input_long_as_float ic =
    let int32_of_char chr = Int32.of_int_exn (int_of_char chr) in
    let long = String.create 4 in
    really_input ic long 0 4;
    let sb1 = Int32.shift_left (int32_of_char long.[0]) 24 in
    let sb2 = Int32.shift_left (int32_of_char long.[1]) 16 in
    let sb3 = Int32.shift_left (int32_of_char long.[2]) 8 in
    let sb4 = int32_of_char long.[3] in
    let result = (Int32.bit_or (Int32.bit_or sb1 sb2) (Int32.bit_or sb3 sb4)) in
    Int32.to_float result
  
  let input_long_as_int ic =
    let f = input_long_as_float ic in
    if f > float_of_int max_int then
      raise (Invalid_file_format
                "read int that cannot be represented as an OCaml native int");
    int_of_float f
  
  let input_list ic ~len ~f =
    let rec loop c lst =
      if c > 0 then loop (c - 1) ((f ic) :: lst)
      else List.rev lst
    in
    loop len []

  let input_array ic ~len ~f = Array.of_list (input_list ic ~len ~f)

  let input_local_time_type ic =
    let gmt_off = input_long_as_float ic in
    let is_dst = bool_of_int (input_byte ic) in
    let abbrv_index = input_byte ic in
    let lt abbrv =
      { Local_time_type.
          gmt_off = gmt_off;
        is_dst = is_dst;
        abbrv = abbrv;
      }
    in
    (lt,abbrv_index)

  let input_leap_seconds ic =
    let leap_time = input_long_as_float ic in
    let seconds = input_long_as_float ic in
    { Leap_second.
        time = leap_time;
      seconds = seconds;
    }

  let input_abbreviations ic ~len =
    let raw_abbrvs = input_list ic ~len ~f:(input_char) in
    let buf = Buffer.create len in
    let _,indexed_abbrvs = List.fold raw_abbrvs ~init:(0, Map.empty)
      ~f:(fun (index,abbrvs) c ->
        match c with
        | '\000' ->
            let data = Buffer.contents buf in
            let next_index = index + (String.length data) + 1 in
            let abbrvs = Map.add abbrvs ~key:index ~data in
            Buffer.clear buf;
            (next_index,abbrvs)
        | c -> Buffer.add_char buf c; (index,abbrvs)
      )
    in
    if Buffer.length buf <> 0 then
      raise
        (Invalid_file_format "missing \000 terminating character in input_abbreviations");
    indexed_abbrvs

  let input_version ic =
    match input_char ic with
    | '\000' -> `V1
    | '2' -> `V2
    | bad_version ->
        raise (Invalid_file_format (sprintf "version (%c) is invalid" bad_version))

  let input_tz_file_v1 ic =
    let module LT = Local_time_type in
    (* space reserved for future use in the format *)
    really_input ic (String.create 15) 0 15;
    let utc_local_count  = input_long_as_int ic in
    let std_wall_count   = input_long_as_int ic in
    let leap_count       = input_long_as_int ic in
    let transition_count = input_long_as_int ic in
    let type_count       = input_long_as_int ic in
    let abbrv_char_count = input_long_as_int ic in
    let transition_times =
      input_list ic ~f:(input_long_as_float) ~len:transition_count
    in
    let transition_indices  = input_list ic ~f:input_byte ~len:transition_count in
    let local_time_types    = input_list ic ~f:(input_local_time_type) ~len:type_count in
    let abbreviations       = input_abbreviations ic ~len:abbrv_char_count in
    let leap_seconds        = input_list ic ~f:(input_leap_seconds) ~len:leap_count in
    (* The following two arrays indicate two boolean values per local_time_type that
       represent a three-value variable that would translate to:
       type transition_type = UTC | Standard | Wall_clock
       However, these are only used by the system library when handling the case where the
       TZ variable is set, not to a time zone name, but instead is of the form:
         TZ = "std offset dst offset, rule"
       Which is deeply obscure, and almost certainly a mistake to use.  This library makes
       no pretense about handling this case.  We continue to read them in for completeness,
       and because it's possible that we will later discover a case where they are used. *)
    let _std_wall_indicators =
      input_array ic ~len:std_wall_count ~f:(fun ic -> bool_of_int (input_byte ic))
    in
    let _utc_local_indicators =
      input_array ic ~len:utc_local_count ~f:(fun ic -> bool_of_int (input_byte ic))
    in
    let local_time_types =
      Array.of_list (List.map local_time_types
        ~f:(fun (lt,abbrv_index) ->
            let abbrv = Map.find_exn abbreviations abbrv_index in
            lt abbrv
          ))
    in
    let transitions =
      List.map2_exn transition_times transition_indices
        ~f:(fun time index ->
          let local_time_type = local_time_types.(index) in
          (time, local_time_type))
    in
    let transitions = Array.of_list transitions in
    let default_local_time_type =
      match Array.find local_time_types ~f:(fun ltt -> not ltt.LT.is_dst) with
      | None -> local_time_types.(0)
      | Some ltt -> ltt
    in
    (fun name digest ->
        {Zone_base.
          name = name;
          file_digest = digest;
          transitions = transitions;
          default_local_time_type = default_local_time_type;
          leap_seconds = leap_seconds;
        }
      )

  let input_tz_file_v2 ic =
    

    input_tz_file_v1 ic

  let input_tz_file zonename filename =
    let check_header ic =
      let buf = String.create 4 in
      really_input ic buf 0 4;
      if buf <> "TZif" then
        raise (Invalid_file_format "magic characters TZif not present")
    in
    try
      protectx (open_in_bin filename) ~finally:(close_in) ~f:(fun ic ->
          check_header ic;
          let make_zone =
            match input_version ic with
            | `V1 -> input_tz_file_v1 ic
            | `V2 -> input_tz_file_v2 ic
          in
          make_zone zonename (Digest.file filename)
        )
    with
    | Invalid_file_format reason ->
        raise (Invalid_file_format (sprintf "%s - %s" filename reason))

  let fill t =
    if not t.full then begin
      let maxdepth = 10 in
      let traverse ~maxdepth dir =
        let rec dfs dir depth =
          if depth < 1 then []
          else
            begin
              let entries = Array.to_list (Sys.readdir dir) in
              List.fold entries ~init:[] ~f:(fun acc fn ->
                  let fn = dir ^ "/" ^ fn in
                  if Sys.is_directory fn = `Yes then
                    List.rev_append (dfs fn (depth - 1)) acc
                  else (fn :: acc)
                )
            end
        in
        dfs dir maxdepth
      in
      let zonefiles = traverse ~maxdepth t.basedir in
      let pos = (String.length t.basedir) + 1 in
      List.iter zonefiles ~f:(fun filename ->
        try
          let len = (String.length filename) - pos in
          let zonename = String.sub filename ~pos ~len in
          (* GMT based timezones in Etc in the posix timezone system are very deceptive
            * and basically shouldn't be used, so we never load them *)
          if not (String.is_prefix ~prefix:"Etc/GMT" zonename) then
            Hashtbl.replace
              t.table ~key:zonename ~data:(input_tz_file zonename filename);
        with
        | _ -> ());
      t.full <- true;
    end

  let default () =
    {
      full = false;
      basedir = "/usr/share/zoneinfo/posix";
      table = String.Table.create ~size:11 ();
    }

  let to_alist t = Hashtbl.to_alist t.table

  let initialized_zones t =
    List.sort ~cmp:(fun a b -> ascending (fst a) (fst b)) (to_alist t)

  let find t zone = Hashtbl.find t.table zone

  let find_exn t zone =
    match find t zone with
    | None -> raise (Unknown_zone zone)
    | Some data -> data

  let find_or_load t zonename =
    let zonename =
      match zonename with
      | "nyc" -> "America/New_York"
      | "lon" | "ldn" -> "Europe/London"
      | "tyo" -> "Asia/Tokyo"
      | "hkg" -> "Asia/Hong_Kong"
      | _     -> zonename
    in
    match find t zonename with
    | Some z -> Some z
    | None ->
        if t.full then None
        else begin
          try
            let filename = t.basedir ^ "/" ^ zonename in
            let zone = input_tz_file zonename filename in
            Hashtbl.replace t.table ~key:zonename ~data:zone;
            Some zone
          with
          | _ -> None
        end
end

let zone_cache = Db.default ()

let init () = Db.fill zone_cache

let initialized_zones () = Db.initialized_zones zone_cache

let string_of_time zone time =
  let local_time = shift_epoch_time UTC (Time.to_float time) zone in
  
  let us =
    Float.iround_exn
      ((Float.Parts.fractional (Float.modf local_time)) *. 1_000_000.)
  in
  let tm = Unix.gmtime local_time in
  sprintf "%d-%0.2d-%0.2d %0.2d:%0.2d:%0.2d.%0.6d" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec us


let time_of_string zone str =
  try
    match String.lsplit2 str ~on:' ' with
    | None -> invalid_arg (sprintf "no space in date_ofday string: %s" str)
    | Some (date,time) ->
        let date = Date.of_string date in
        let ofday = Ofday.of_string time in
        let epoch_time =
          utc_mktime ~year:(Date.year date) ~month:(Month.to_int (Date.month date))
            ~day:(Date.day date) ~hour:0 ~min:0 ~sec:0 ~msec:0 in
        let epoch_time = epoch_time +. Ofday.to_sec ofday in
        let utc_time = shift_epoch_time Localtime epoch_time zone in
        Time.of_float utc_time
  with e ->
    Exn.reraise e "TZ.time_of_string"

module Zone = struct
  include Zone_base

  let abbreviation zone time =
    (find_local_time_type (Time.to_float time) zone UTC).Local_time_type.abbrv

  let of_gmt_offset offset =
    assert (offset >= -24 && offset <= 24);
    let name = sprintf "GMT%s%d" (if offset < 0 then "-" else "+") (abs offset) in
    {
      name = name;
      file_digest = Digest.string (sprintf "GMT %d" offset);
      transitions = [||];
      default_local_time_type = {Local_time_type.
        gmt_off = Float.of_int (offset * 60 * 60);
        is_dst = false;
        abbrv = name;
      };
      leap_seconds = []
    }
  
  let to_gmt_offset t =
    let ltt = t.default_local_time_type in
    Int.of_float ltt.Local_time_type.gmt_off
  
  let find zone = Db.find_or_load zone_cache zone

  let find_exn zone =
    match find zone with
    | None -> raise (Unknown_zone zone)
    | Some z -> z

  let to_string_hum t = Sexp.to_string_hum (sexp_of_t t)

  let t_of_sexp sexp =
    match sexp with
    | Sexp.Atom name ->
        begin
          try
            if
              
              String.is_prefix name ~prefix:"GMT" || String.is_prefix name ~prefix:"GMT+"
            then begin
              let offset =
                Int.of_string (String.sub name ~pos:4 ~len:(String.length name - 4))
              in
              let offset =
                match name.[3] with
                | '-' -> (-1) * offset
                | '+' -> offset
                | _   -> assert false
              in
              of_gmt_offset offset
            end else find_exn name
          with exc ->
            of_sexp_error
              (sprintf "TZ.Zone.t_of_sexp: %s" (Exn.to_string exc)) sexp
        end
    | _ -> of_sexp_error "TZ.Zone.t_of_sexp: expected atom" sexp
  
  let sexp_of_t t = Sexp.Atom t.name

  let utc = of_gmt_offset 0
end

let machine_zone () =
  (* There are a number of equivalant timezone names in the POSIX
     database, but if possible we would prefer these name choices over
     others. *)
  let best_choices = Set.of_list [
      "America/New_York";
      "Europe/London";
      "Asia/Tokyo";
      "Asia/Hong_Kong";
    ]
  in
  Db.fill zone_cache;
  match Sys.getenv "TZ" with
  | Some tz -> (try Some (tz, Zone.find_exn tz) with _ -> None)
  | None ->
      let digest = Digest.file "/etc/localtime" in
      let possibilities =
        List.filter (Db.to_alist zone_cache) ~f:(fun (_name, tz) ->
          tz.Zone.file_digest = digest)
      in
      match possibilities with
      | [] -> None
      | (x :: _) as lst ->
          match List.find lst ~f:(fun (name,_) -> Set.mem best_choices name) with
          | None -> Some x
          | res -> res

let machine_zone_exn db =
  match machine_zone db with
  | None -> failwith "Unable to determine timezone for this machine"
  | Some res -> res
