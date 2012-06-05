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

(** A bag is a data structure like a set, except that:
 *
 *   - It doesn't require anything (hashable, comparable) of elements in the bag.
 *   - Duplicates are allowed.
 *   - Addition and removal are constant time.
 *
 * It is an error to modify a bag (add, remove, remove_one, ...) during iteration
 * (fold, iter, ...).
 *)
open Std_internal

module Elt : sig
  type 'a t

  val equal : 'a t -> 'a t -> bool
  val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
  val value : 'a t -> 'a
end

type 'a t

include Container.S1 with type 'a container = 'a t

val invariant : 'a t -> unit

(** [create ()] returns an empty bag. *)
val create : unit -> 'a t

(** [add t v] adds [v] to the bag [t], returning an element that can
    later be removed from the bag.  [add] runs in constant time.
*)
val add : 'a t -> 'a -> 'a Elt.t


(** [remove t elt] removes [elt] from the bag [t], raising an exception if [elt]
    is not in the bag.  [remove] runs in constant time.
*)
val remove : 'a t -> 'a Elt.t -> unit

(** [some_elt t] returns some element in the bag. *)

val some_elt : 'a t -> 'a Elt.t option

(** [remove_one t] removes some element from the bag, and returns its value.
    [remove_one] runs in constant time.
*)
val remove_one : 'a t -> 'a option

(** [clear t] removes all elements from the bag.  [clear] runs in O(1) time. *)
val clear : 'a t -> unit

(** [find_elt t ~f] looks at elements in the bag one-by-one until it finds one
    [elt] such that [f (Elt.value elt)], in which case it returns [Some elt].
    If there is no element satisfying [f], then [find_elt] returns [None]. *)
val find_elt : 'a t -> f:('a -> bool) -> 'a Elt.t option

(** [until_empty t f] repeatedly removes a value [v] from [t] and runs [f v],
    continuing until [t] is empty.  Running [f] may add elements to [t] if it
    wants.
*)
val until_empty : 'a t -> ('a -> unit) -> unit

val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
