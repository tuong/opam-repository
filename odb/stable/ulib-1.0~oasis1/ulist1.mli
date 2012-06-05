(***********************************************************************)
(*                                                                     *)
(*                              My_stdlib                              *)
(*                                                                     *)
(*             projets Estime & Cristal, INRIA-Rocquencourt            *)
(*                                                                     *)
(*  Copyright 2004-2007 INRIA.                                         *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved. This file is distributed only by permission.  *)
(*                                                                     *)
(*  Francois Clement <Francois.Clement@inria.fr>                       *)
(*  Pierre Weis <Pierre.Weis@inria.fr>                                 *)
(*                                                                     *)
(***********************************************************************)

(* $Id: ulist1.mli,v 1.2 2009-12-12 17:40:15 fclement Exp $ *)

(** The module defining nonempty lists. *)

type 'a list1 = 'a * 'a list;;

type 'a t = 'a list1;;

val length : 'a list1 -> int;;
(** Return the length (number of elements) of the given list. *)

val to_list : 'a list1 -> 'a list;;
(** [to_list l] returns the (usual) list of all the elements of the
  nonempty list [l]. *)

val of_list : 'a list -> 'a list1;;
(** [of_list l] returns the nonempty list containing the elements of
   [l].
   Raise [Invalid_argument] if the list is empty. *)

val concat : 'a list list1 -> 'a list1;;
(** Concatenate a list of lists.  The elements of the argument are all
   concatenated together (in the same order) to give the result.
   Not tail-recursive
   (length of the argument + length of the longest sub-list). *)

val replace_by_list : int -> 'a list1 -> 'a list1 -> 'a list1;;
(** [replace_by_list i l1 l2] replaces the [i]-th element
   of the second list [l2] by the elements of the first list [l1]
   (in the same order). *)

val iter : ('a -> unit) -> 'a list1 -> unit
(** [iter f [a1; ...; an]] applies function [f] in turn to
   [a1; ...; an]. It is equivalent to
   [begin f a1; f a2; ...; f an; () end]. *)

val map : ('a -> 'b) -> 'a list1 -> 'b list1
(** [map f [a1; ...; an]] applies function [f] to [a1, ..., an],
   and builds the list [[f a1; ...; f an]]
   with the results returned by [f].  Not tail-recursive. *)

val map_i : (int -> 'a -> 'b) -> 'a list1 -> 'b list1;;
(** [map_i f l] returns the same result as {!map},
   but the function is applied to the index of the element as first argument,
   and the element itself as second argument.
   Elements of the list [l] are implicitely numbered from 0 to
   [(] {!length} [l) - 1].
   
   Notice that this could mean that you actually need an array, not a list!
   Hence, you might think of using {!Array.mapi}... *)

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list1 -> 'a
(** [fold_left f a [b1; ...; bn]] is
   [f (... (f (f a b1) b2) ...) bn]. *)

val fold_right : ('a -> 'b -> 'b) -> 'a list1 -> 'b -> 'b
(** [fold_right f [a1; ...; an] b] is
   [f a1 (f a2 (... (f an b) ...))].  Not tail-recursive. *)

val iter2 : ('a -> 'b -> unit) -> 'a list1 -> 'b list1 -> unit
(** [iter2 f [a1; ...; an] [b1; ...; bn]] calls in turn
   [f a1 b1; ...; f an bn].
   Raise [Invalid_argument] if the two lists have
   different lengths. *)

val map2 : ('a -> 'b -> 'c) -> 'a list1 -> 'b list1 -> 'c list1
(** [map2 f [a1; ...; an] [b1; ...; bn]] is
   [[f a1 b1; ...; f an bn]].
   Raise [Invalid_argument] if the two lists have
   different lengths.  Not tail-recursive. *)

val find : ('a -> bool) -> 'a list1 -> 'a
(** [find p l] returns the first element of the list [l]
   that satisfies the predicate [p].
   Raise [Not_found] if there is no value that satisfies [p] in the
   list [l]. *)

val filter : ('a -> bool) -> 'a list1 -> 'a list
(** [filter p l] returns all the elements of the list [l]
   that satisfy the predicate [p].  The order of the elements
   in the input list is preserved.  *)

val partition : ('a -> bool) -> 'a list1 -> 'a list1 * 'a list1;;
(** [partition p l] returns a pair of nonempty lists [(l1, l2)], where
   [l1] is the list of all the elements of [l] that satisfy the predicate [p],
   and [l2] is the list of all the elements of [l] that do not satisfy [p].
   The order of the elements in the input list is preserved.
   Raise [Invalid_argument] if [l1] or [l2] would be empty. *)

(** {6 Association lists} *)

val max_snd_assoc : ('a -> 'a -> int) -> ('b * 'a) list1 -> 'b * 'a;;
(** [max_snd_assoc l] returns the leftmost element of the list of pairs
   [l] with the maximum second component. *)

