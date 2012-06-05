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

(** An implementation of compressed integer sets using lists of integer ranges. Operations
    such as adding and membership are O(n) where n is the number of contigous ranges in
    the set. For data that is mostly serial, n should remain very small.

    Note that when n gets very large, in addition to poor performance, this behavior may
    throw exceptions since some of the code is not tail-recursive.
*)

type t

val create : unit -> t

val to_string : t -> string

(** [add_range t i j] add all the numbers between [i] and [j] (inclusive) to the set. Note
    that it doesn't matter which order [i] and [j] are specified in, the effect is the
    same. *)
val add_range : t -> int -> int -> t

(** [add t i] add [i] to the set *)
val add : t -> int -> t

(** [mem t i] test whether [i] is a member of the set *)
val mem : t -> int -> bool

(** [ranges t] return a list of all ranges that make up the set *)
val ranges : t -> (int * int) list

(** [max t] the biggest number in the set (if it exists) *)
val max : t -> int option

(** [min t] the smallest number in the set (if it exists) *)
val min : t -> int option


