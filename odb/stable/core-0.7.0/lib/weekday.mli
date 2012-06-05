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



type t

include Binable.S with type binable = t
include Hashable.S with type hashable = t
include Sexpable.S with type sexpable = t
include Comparable.S with type comparable = t

(** [of_string s] accepts three-character abbreviations with any capitalization
*)
include Stringable.S with type stringable = t

val invariant : t -> unit

val sun : t
val mon : t
val tue : t
val wed : t
val thu : t
val fri : t
val sat : t

type variant = [ `Sun | `Mon | `Tue | `Wed | `Thu | `Fri | `Sat ]

val get : t -> variant

val create : variant -> t

(** conversion between ints and weekdays uses the same mapping as Unix.tm_wday.
    0 <-> Sun, ... 6 <-> Sat *)

(** [of_int i] returns i'th weekday if [i] is in 0,1,...,6.  Otherwise it returns
    None. *)
val of_int : int -> t option

(** [of_int_exn i] should have i in 0,1,...,6 and returns the i'th weekday. *)
val of_int_exn : int -> t

(** [to_int t] returns an int in 0,1,...6. *)
val to_int : t -> int

(** [shift t i] goes forward (or backward) the specified number of weekdays *)
val shift : t -> int -> t

(** [is_sun_or_sat] returns true if t is Sunday or Saturday *)
val is_sun_or_sat : t -> bool
