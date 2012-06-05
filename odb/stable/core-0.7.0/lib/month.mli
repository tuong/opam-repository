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
include Comparable.S with type comparable = t
include Hashable.S with type hashable = t
include Sexpable.S with type sexpable = t

(** [sexp_of_t_mode] controls how months are converted to strings by [sexp_of_t].
 *    `Upper       JAN
 *    `Lower       jan
 *    `Capitalized Jan
 *
 * The default is `Upper
 *)

val sexp_of_t_mode : [ `Upper | `Lower | `Capitalized ] ref

(**
 * [of_string s] accepts three-character abbreviations with any capitalization
 *)
include Stringable.S with type stringable = t

val invariant : t -> unit

val jan : t
val feb : t
val mar : t
val apr : t
val may : t
val jun : t
val jul : t
val aug : t
val sep : t
val oct : t
val nov : t
val dec : t

val all : t list


type variant = [ `Jan | `Feb | `Mar | `Apr | `May | `Jun
               | `Jul | `Aug | `Sep | `Oct | `Nov | `Dec ]

val get : t -> variant

val create : variant -> t

(** [of_int i] returns i'th month if [i] is in 1,2,...,12.  Otherwise it returns
    None. *)
val of_int : int -> t option

(** [of_int_exn i] should have i in 1,2,...,12 and returns the i'th month. *)
val of_int_exn : int -> t

(** [to_int t] returns an int in 1,2,...12. *)
val to_int : t -> int

(** [shift t i] goes forward (or backward) the specified number of months *)
val shift : t -> int -> t
