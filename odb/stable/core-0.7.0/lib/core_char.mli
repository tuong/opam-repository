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

(** Character operations. *)

(** An alias for the type of characters. *)
type t = char

include Comparable.S with type comparable = t
include Sexpable.S with type sexpable = t
include Hashable.S with type hashable = t

(** Return the ASCII code of the argument. *)
val to_int : t -> int

(** Return the character with the given ASCII code or [None] is the argument is outside
    the range 0 to 255. *)
val of_int : int -> t option

(** Return the character with the given ASCII code.  Raise [Failure] if the argument is
    outside 0 to 255. *)
val of_int_exn : int -> t

val unsafe_of_int : int -> t

(** Return a string representing the given character, with special characters escaped
    following the lexical conventions of Objective Caml. *)
val escaped : t -> string

(** Convert the given character to its equivalent lowercase character. *)
val lowercase : t -> t

(** Convert the given character to its equivalent uppercase character. *)
val uppercase : t -> t

val to_string : t -> string

(** '0' - '9' *)
val is_digit : t -> bool

(** 'a' - 'z' *)
val is_lowercase : t -> bool

(** 'A' - 'Z' *)
val is_uppercase : t -> bool

(** 'a' - 'z' or 'A' - 'Z' *)
val is_alpha : t -> bool

(** 'a' - 'z' or 'A' - 'Z' or '0' - '9' *)
val is_alphanum : t -> bool

(** ' ' - '~' *)
val is_print : t -> bool

(** ' ' or '\t' or '\r' or '\n' *)
val is_whitespace : t -> bool

(** Return [Some i] if [is_digit c] and [None] otherwise. *)
val get_digit : t -> int option

(** Return [i] if [is_digit c].  Raises [Failure] otherwise. *)
val get_digit_exn : t -> int

val min_value : t
val max_value : t
