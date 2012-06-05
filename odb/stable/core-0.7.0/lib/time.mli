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

(** Our time module.  This module wraps up unix times, including various
    convenience functions for accessing them.
*)
open Common
open Std_internal

(** A date+time, represented Unix-style, as a number of seconds since 1970-01-01.  The
    representation is a time-in-the-universe; not a time-in-some-timezone. *)
type t = Time_internal.T.t

(** To be used in Sexp when we want a UTC time instead of a local time.*)
module UTC : sig
  include Sexpable with type sexpable = t
  include Stringable with type stringable = t
  type t = sexpable
end

include Hashable_binable with type hashable = t
include Comparable_binable with type comparable = t
include Robustly_comparable with type robustly_comparable = t
include Sexpable with type sexpable = t
include Binable with type binable = t
include Stringable with type stringable = t
include Floatable with type floatable = t (* seconds since the epoch *)

(** {values} *)

(* midnight, Jan 1, 1970 in UTC *)
val epoch : t

(** {6 Basic operations on times} *)

(** [add t s] adds the span [s] to time [t] and returns the resulting time.

    NOTE: adding spans as a means of adding days is not accurate, and may run into trouble
    due to shifts in daylight savings time, float arithmetic issues, and leap seconds.
    See the comment at the top of TZ.mli for a more complete discussion of some of the
    issues of time-keeping.  For spans that cross date boundaries, use date functions
    instead.
*)
val add : t -> Span.t -> t

(** [sub t s] subtracts the span [s] from time [t] and returns the
    resulting time.  See important note for [add]. *)
val sub : t -> Span.t -> t

(** [diff t1 t2] returns time [t1] minus time [t2]. *)
val diff : t -> t -> Span.t

(** [diff t1 t2] returns the absolute span of time [t1] minus time [t2]. *)
val abs_diff : t -> t -> Span.t

(** {6 Constants} *)

(** {6 Conversions} *)
(** All these conversion functions use the current time zone. Unless marked _utc,
    in which case they use Universal Coordinated Time *)


val of_date_ofday : Date.t -> Ofday.t -> t
val to_date_ofday : t -> Date.t * Ofday.t

(** Assume the specified date and time of day are UTC *)
val of_date_ofday_utc : Date.t -> Ofday.t -> t

(** Produce the current UTC date and time of day *)
val to_date_ofday_utc : t -> Date.t * Ofday.t

val to_date : t -> Date.t
val to_ofday : t -> Ofday.t


(** Other string conversions  *)
(** [to_filename_string t] converts [t] to string with format YYYY-MM-DD_HH-MM-SS.mmm
    which is suitable for using in filenames *)
val to_filename_string : t -> string
(** [of_filename_string s] converts [s] that has format YYYY-MM-DD_HH-MM-SS.mmm into time *)
val of_filename_string : string -> t


val to_string_fix_proto : [`Utc | `Local] -> t -> string
val of_string_fix_proto : [`Utc | `Local] -> string -> t

val to_string_old : t -> string  (* MM/DD/YYYY HH:MM:SS.MS *)

(** [to_string_trimmed t] Same as to_string, but removes trailing seconds and
  milliseconds if they are 0 *)
val to_string_trimmed : t -> string

(** [to_sec_string t] Same as to_string, but without milliseconds *)
val to_sec_string : t -> string

(* The following two functions are suggested as a more robust string
   representation of time by absolute time. It supports the exchange of time represented
   as a string across processes, regardless their local, nor processing any other Time
   Zone metadata (eg. daylight time savings or political decisions).
*)

(** [of_string_abs t] accepts a string with format:
    YYYY-MM-DD(T| )hh:mm:ss[.ss][Z|((+|-)hh:mm)]. If the offset or Z is missing,
    then it is assumed to be the machine's local time. *)
val of_string_abs : string -> t

(** [to_string_abs t] creates a string with format:
    YYYY-MM-DDThh:mm:ss.ssZ (ie. GMT time). *)
val to_string_abs : t -> string



val of_date_time_strings : string -> string -> t
val of_date_time_strings_utc : string -> string -> t

val pp : Format.formatter -> t -> unit

(** {6 Miscellaneous} *)

(** @return the current time. *)
val now : unit -> t

(** Pause (and don't throw an exception) *)
val pause : Span.t -> unit
val pause_forever : unit -> never_returns


(** [ ofday_occurrence ofday side now ] returns a Time.t that is the occurrence of ofday
    (in local time) which is the latest occurrence before now or the earliest occurrence
    after now, according to side.  NOTE: This function is a little bit wrong near daylight
    savings time *)
val ofday_occurrence : Ofday.t -> [ `right_after | `right_before ] -> t -> t

(** [ ofday_occurrence ofday side now ] returns a Time.t that is the occurrence of ofday
    (in UTC) which is the latest occurrence before now or the earliest occurrence after
    now, according to side.  NOTE: This function is a little bit wrong near daylight
    savings time *)
val ofday_occurrence_utc : Ofday.t -> [ `right_after | `right_before ] -> t -> t

(** [ format t fmt ] formats the given time according to fmt, which follows the formatting
    rules given in 'man strftime' *)
val format : t -> string -> string 

