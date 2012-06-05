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

(** doubly-linked lists *)

(**
 * There is a fundamental problem with a data structure (like doubly-linked
 * lists) that is both mutable and provides iteration function that call back
 * to user-supplied functions.  If those user-supplied functions modify the
 * data structure, what is the semantics of the remainder of the iteration?
 * This module sidesteps this issue by detecting attempts by user-supplied
 * functions to modify a doubly-linked list while in the middle of iterating
 * over it.
 *
 * Modification functions include:
 *   insert_*, remove*, transfer
 * Iteration functions include:
 *   exists, fold*, for_all, find
 *
 * Calls to modification functions detect if the list is being iterated over,
 * and if so raise an exception rather than modify the list.  For example, a
 * use like the following would raise.
 *
 *  iter t ~f:(fun _ -> ... remove t e ...)
 *)

open Sexplib

module Elt : sig
  type 'a t

  val value : 'a t -> 'a
  val equal : 'a t -> 'a t -> bool (* pointer equality *)
  val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
end

type 'a t

include Container.S1 with type 'a container = 'a t
include Sexpable.S1 with type 'a sexpable = 'a t

val invariant : 'a t -> unit

(** creating doubly-linked lists *)
val create : unit -> 'a t

(** [of_list l] returns a doubly-linked list [t] with the same elements as [l]
    and in the same order (i.e. the first element of [l] is the first element
    of [t]).  It is always the case that [l = to_list (of_list l)].
*)
val of_list : 'a list -> 'a t

(** predicates *)
val equal : 'a t -> 'a t -> bool (* pointer equality *)
val is_first : 'a t -> 'a Elt.t -> bool
val is_last : 'a t -> 'a Elt.t -> bool

(** constant-time extraction of first and last elements. *)
val first_elt : 'a t -> 'a Elt.t option
val last_elt : 'a t -> 'a Elt.t option
val first : 'a t -> 'a option
val last : 'a t -> 'a option

(** constant-time move to next or previous element. *)
val next : 'a t -> 'a Elt.t -> 'a Elt.t option
val prev : 'a t -> 'a Elt.t -> 'a Elt.t option

(** constant-time insertion of a new element.
    It is an error to call [insert_before t e a] or [insert_after t e a] if [e]
    is not an element in [t], and will break invariants.
 *)
val insert_before : 'a t -> 'a Elt.t -> 'a -> 'a Elt.t
val insert_after : 'a t -> 'a Elt.t -> 'a -> 'a Elt.t
val insert_first : 'a t -> 'a -> 'a Elt.t
val insert_last : 'a t -> 'a -> 'a Elt.t

(** constant-time removal of an element.
    It is an error to call [remove t e] when [e] is not in [t], and will break
    [invariant].
 *)
val remove : 'a t -> 'a Elt.t -> unit
val remove_first : 'a t -> 'a option
val remove_last : 'a t -> 'a option

(** [find_elt t ~f] finds the first element in [t] that satisfies [f], by
    testing each of element of [t] in turn until [f] succeeds.
*)
val find_elt : 'a t -> f:('a -> bool) -> 'a Elt.t option

(** [clear t] removes all elements from the list in constant time. *)
val clear : 'a t -> unit

(** [copy t] returns a copy of [t]. *)
val copy : 'a t -> 'a t

(** [transfer ~src ~dst] has the same behavior as
    [iter src ~f:(insert_last dst); clear src]
    except that it runs in constant time.

    If [s = to_list src] and [d = to_list dst], then after [transfer ~src ~dst]:
      [to_list src = []]
      [to_list dst = d @ s]
*)
val transfer : src:'a t -> dst:'a t -> unit

(** [filter_inplace t ~f] removes all elements of [t] that don't satisfy [f]. *)
val filter_inplace : 'a t -> f:('a -> bool) -> unit
