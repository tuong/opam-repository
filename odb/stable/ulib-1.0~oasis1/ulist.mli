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

(* $Id: ulist.mli,v 1.3 2009-06-22 09:11:29 fclement Exp $ *)

(** More list operations. *)

val printer : 'a Uprinter.t -> 'a list Uprinter.t;;
(** [printer print ob l] prints the list [l] on the output buffer [ob]
    by evaluating [print ob] on all elements of the list. *)

val replace : int -> 'a -> 'a list -> 'a list;;
(** [replace i x l] replaces [i]-th element of the list [l] by [x].
   
   Notice that this could mean that you actually need an array, not a list!
   Hence, you should think of using {!Array.set}... *)

val rev_replace : int -> 'a -> 'a list -> 'a list;;
(** [rev_replace i x l] gives the same result as
   {!List.rev} [(]{!replace} [i x l)], but is tail-recursive
   and more efficient. *)

val replace_by_list : int -> 'a list -> 'a list -> 'a list;;
(** [replace_by_list i l1 l2] replaces the [i]-th element
   of the second list [l2] by the elements of the first list [l1]
   (in the same order). *)

val rev_replace_by_list : int -> 'a list -> 'a list -> 'a list;;
(** [rev_replace_by_list i l1 l2] gives the same result as
   {!List.rev} [(]{!replace_by_list} [i l1 l2)], but is tail-recursive
   and more efficient. *)

val insert : int -> 'a list -> 'a list -> 'a list;;
(** [insert i l1 l2] inserts the elements of the first list [l1]
   (in the same order) right before the [i]-th element of the second list [l2].
   Hence, [insert 0] is the same as {!List.append}. *)

val rev_insert : int -> 'a list -> 'a list -> 'a list;;
(** [rev_insert i l1 l2] gives the same result as
   {!List.rev} [(]{!insert} [i l1 l2)], but is tail-recursive
   and more efficient. *)

(** {6 Iterators} *)

val map_i : (int -> 'a -> 'b) -> 'a list -> 'b list;;
(** [map_i f l] returns the same result as {!List.map},
   but the function is applied to the index of the element as first argument,
   and the element itself as second argument.
   Elements of the list [l] are implicitely numbered from 0 to
   [(] {!List.length} [l) - 1].
   
   Notice that this could mean that you actually need an array, not a list!
   Hence, you might think of using {!Array.mapi}... *)

val rev_map_i : (int -> 'a -> 'b) -> 'a list -> 'b list;;
(** [rev_map_i f l] gives the same result as
   {!List.rev} [(]{!map_i} [f l)], but is tail-recursive
   and more efficient. *)

val fold_left_i : (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> 'a;;
(** [fold_left_i f l] returns the same result as {!List.map},
   but the function is applied to the index of the element as first argument,
   and the element itself as second argument.
   Elements of the list [l] are implicitely numbered from 0 to
   [(] {!List.length} [l) - 1].
   
   Notice that this could mean that you actually need an array, not a list!
   Hence, you might think of using {!Array.mapi}... *)

(** {6 Association lists} *)

val min_assoc : ('a -> 'a -> int) -> ('a * 'b) list -> 'a * 'b;;
(** [min_assoc l] returns the leftmost element of the list of pairs [l]
   with the minimum first component.
   Raise [Invalid_argument] if the list is empty. *)

val max_assoc : ('a -> 'a -> int) -> ('a * 'b) list -> 'a * 'b;;
(** [max_assoc l] returns the leftmost element of the list of pairs [l]
   with the maximum first component.
   Raise [Invalid_argument] if the list is empty. *)

val min_fst_assoc : ('a -> 'a -> int) -> ('a * 'b) list -> 'a * 'b;;
(** Another name for {!min_assoc}. *)

val max_fst_assoc : ('a -> 'a -> int) -> ('a * 'b) list -> 'a * 'b;;
(** Another name for {!max_assoc}. *)

val min_snd_assoc : ('a -> 'a -> int) -> ('b * 'a) list -> 'b * 'a;;
(** [min_snd_assoc l] returns the leftmost element of the list of pairs [l]
   with the minimum second component.
   Raise [Invalid_argument] if the list is empty. *)

val max_snd_assoc : ('a -> 'a -> int) -> ('b * 'a) list -> 'b * 'a;;
(** [max_snd_assoc l] returns the leftmost element of the list of pairs [l]
   with the maximum second component.
   Raise [Invalid_argument] if the list is empty. *)

val rank : 'a -> 'a list -> int;;
(** [rank e l] returns [r] such that [List.nth l r] = [e]. *)
