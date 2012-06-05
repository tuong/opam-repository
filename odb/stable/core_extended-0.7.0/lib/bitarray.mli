(******************************************************************************
 *                             Core-extended                                  *
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

(** This module implements efficient and compact arrays of boolean values. It stores it's
    values in the bits of an integer, using multiple integers to allow for arrays larger
    than the machine word size. All operations are on immediates (no caml_modify), and are
    quite simple. Hence this data structure should be more efficient than an array of
    bools. *)

type t

include Core.Std.Sexpable with type sexpable = t

(** [create size] size must be less than ((word size - 2) * max array length) *)
val create : int -> t

(** [get t pos] get the value in position [pos], raises Invalid_argument if the position
    is out of bounds. *)
val get : t -> int -> bool

(** [set t pos] set the value in position [pos], raises Invalid_argument if the position
    is out of bounds. *)
val set : t -> int -> bool -> unit

(** [clear t] set the contents of every element to false O(n / (word_size - 2)) *)
val clear : t -> unit

(** [fold t ~init ~f] Fold over the array as in [Array.fold] *)
val fold : t -> init:'a -> f:('a -> bool -> 'a) -> 'a

(** [iter t ~f] Iterate over the array as in [Array.iter] *)
val iter : t -> f:(bool -> unit) -> unit
