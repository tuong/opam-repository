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

(* Represented as a number of seconds since midnight *)
type t = private float

include Sexpable with type sexpable = t
include Binable with type binable = t
include Comparable_binable with type comparable = t
include Hashable_binable with type hashable = t
include Robustly_comparable with type robustly_comparable = t
include Stringable with type stringable = t
include Floatable with type floatable = t (* seconds since midnight *)

val create : ?hr:int -> ?min:int -> ?sec:int -> ?ms:int -> unit -> t

val to_parts : t -> Span.Parts.t

val to_span_since_midnight : t -> Span.t
val of_span_since_midnight : Span.t -> t

val start_of_day : t
val end_of_day : t

(*val now : unit -> t*)

(** [add t s] shifts the time of day [t] by the span [s].  It returns None if
    the result is not in the same day.
*)
val add : t -> Span.t -> t option
val sub : t -> Span.t -> t option
(* Takes the difference between two ofdays, as if they were times occurring on the same
    day. *)
val diff : t -> t -> Span.t

(** since midnight *)
val to_sec : t -> float
val of_sec : float -> t

(* We have two times which try to represent the same moment
    but may be off a little bit (<30min) and which may use different timezone
    we need to figure out how much they differ.

    If one of the [t] is [min_value] or [max_value], the result is [None]
    to avoid the [nan] problem. Be careful [None] does not mean "no diff".
*)
val small_diff : t -> t -> Span.t option

val pp : Format.formatter -> t -> unit

(** [to_string_trimmed t] return a string with trailing seconds and subseconds
  trimmed off if they are 0 *)
val to_string_trimmed : t -> string

(** [to_sec_string t] returns a string with trailing milliseconds trimmed*)
val to_sec_string : t -> string

val of_string_iso8601_extended : ?pos:int -> ?len:int -> string -> t

val to_millisec_string : t -> string
