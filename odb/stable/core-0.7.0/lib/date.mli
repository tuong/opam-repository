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

type t = { y: int; m: Month.t; d: int; }

include Sexpable with type sexpable = t
include Binable with type binable = t
include Hashable_binable with type hashable = t
(** converts a string to a date, in formats:
  * m/d/y
  * y-m-d (* valid iso8601_extended *)
  * DD MMM YYYY
  * DDMMMYYYY
  * YYYYMMDD *)
include Stringable with type stringable = t
include Comparable_binable with type comparable = t

val create : y:int -> m:Month.t -> d:int -> t

val of_tm : Core_unix.tm -> t
val of_time : Time_internal.T.t -> t

(* For details on this ISO format, see:

    http://www.wikipedia.org/wiki/iso8601
*)
val to_string_iso8601_extended : t -> string (* YYYY-MM-DD *)

val of_string_iso8601_basic : string -> pos:int -> t (* YYYYMMDD *)
val to_string_iso8601_basic : t -> string            (* YYYYMMDD *)

val to_string_old : t -> string              (* MM/DD/YYYY *)

val pp : Format.formatter -> t -> unit
val day : t -> int
val month : t -> Month.t
val year : t -> int

val today : unit -> t

val day_of_week : t -> Weekday.t

val is_weekend : t -> bool
val is_weekday : t -> bool

(* Monday through Friday are business days, unless they're a holiday *)
val is_business_day : t -> is_holiday:(t -> bool) -> bool

val add_days : t -> int -> t

(** [add_months t n] returns date with max days for the month if the date would be
    invalid. e.g. adding 1 month to Jan 30 results in Feb 28 due to Feb 30 being
    an invalid date, Feb 29 is returned in cases of leap year. **)
val add_months : t -> int -> t

(** [diff t1 t2] returns date [t1] minus date [t2] in days. *)
val diff : t -> t -> int

(** [add_weekdays t 0] returns the next weekday if [t] is a weekend and [t]
    otherwise *)
val add_weekdays : t -> int -> t

(** [add_business_days t ~is_holiday n] returns a business day even when
    n=0. [add_business_days ~is_holiday:(fun _ -> false) ...] is the same as
    [add_weekdays]. Use [Pnl_db.Calendar_events.is_holiday] as a conveninent
    holiday function. *)
val add_business_days : t -> is_holiday:(t -> bool) -> int -> t

(* the following returns a closed interval (endpoints included) *)
val dates_between : min:t -> max:t -> t list

val business_dates_between : min:t -> max:t -> is_holiday:(t -> bool) -> t list

val weekdays_between : min:t -> max:t -> t list

val previous_weekday : t -> t

module Export : sig
  type _date = t = { y: int; m: Month.t; d: int; } 
end
