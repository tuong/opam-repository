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

(* $Id: uarray.mli,v 1.2 2007-07-13 12:53:27 fclement Exp $ *)

(** More array operations. *)

(** {6 Filling arrays} *)

val array_set_sub : ('a -> 'a) -> 'a array -> int -> int -> unit;;
(** [array_set_sub f a ofs len] applies function [f] in turn to the
   elements number [ofs] to [ofs + len - 1] of array [a] and stores the result
   to the corresponding location. It is equivalent to
   [a.(ofs) <- f a.(ofs);
    a.(ofs + 1) <- f a.(ofs + 1);
    ...;
    a.(ofs + len - 1) <- f a.(ofs + len - 1)].

   Raise [Invalid_argument] if [ofs] and [len] do not designate a valid
   subarray of [a]. *)

val array_set : ('a -> 'a) -> 'a array -> unit;;
(** [array_set f a] is equivalent to
   {!array_set_sub} [f a 0 (Array.length a)]. *)

val array_seti_sub : (int -> 'a -> 'a) -> 'a array -> int -> int -> unit;;
(** Same as {!array_set_sub}, but the function is applied to the index
   of the element as first argument, and the element itself as second argument.
   *)

val array_seti : (int -> 'a -> 'a) -> 'a array -> unit;;
(** [array_seti f a] is equivalent to
   {!array_seti_sub} [f a 0 (Array.length a)]. *)

(** {6 Printers} *)

val column_printer : 'a Uprinter.t -> 'a array Uprinter.t;;
(** [column_printer print ob a] prints the array [a] on the output
    buffer [ob] as a column by evaluating [print ob] on all elements of the
    array. *)

val line_printer : 'a Uprinter.t -> 'a array Uprinter.t;;
(** Same as [column_printer] but with line output. *)

val compact_printer : 'a Uprinter.t -> 'a array Uprinter.t;;
(** Same as [line_printer] but with compact output. *)

val list_of_list_array : 'a list array -> 'a array list;;
(** *)

val list_to_list_array : 'a array list -> 'a list array;;
(** *)

(** {6 Iterators} *)

val subiter : ('a -> unit) -> 'a array -> int -> int -> unit;;
(** [subiter f a ofs len] applies function [f] in turn to the
   elements number [ofs] to [ofs + len - 1] of array [a]. It is equivalent to
   [f a.(ofs); f a.(ofs + 1); ...; f a.(ofs + len - 1); ()].

   Raise [Invalid_argument] if [ofs] and [len] do not designate a valid
   subarray of [a]. *)

val subiteri : (int -> 'a -> unit) -> 'a array -> int -> int -> unit;;
(** Same as {!subiter}, but the function is applied to the index of
   the element as first argument, and the element itself as second argument. *)

(** {6 Iterators on two arrays} *)

val iter2 : ('a -> 'b -> unit) -> 'a array -> 'b array -> unit;;
(** [iter2 f a b] applies function [f] in turn to all the elements
   of [a] and [b]. It is equivalent to
   [f a.(0) b.(0); f a.(1) b.(1); ...; f a.(n - 1) b.(n - 1); ()],
   where [n] is the length of arrays [a] and [b].
   
   Raise [Invalid_argument] if the two arrays have different lengths. *)

val map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array;;
(** [map2 f a b] applies function [f] to all the elements of [a]
   and [b], and builds an array with the results returned by [f]:
   [[| f a.(0) b.(0); f a.(1) b.(1); ...; f a.(n - 1) b.(n - 1) |]],
   where [n] is the length of arrays [a] and [b].
   
   Raise [Invalid_argument] if the two arrays have different lengths. *)

val iteri2 : (int -> 'a -> 'b -> unit) -> 'a array -> 'b array -> unit;;
(** Same as {!iter2}, but the function is applied to the index of the
   elements as first argument, and the elements themselves as second and third
   arguments. *)

val mapi2 : (int -> 'a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array;;
(** Same as {!map2}, but the function is applied to the index of the
   elements as first argument, and the elements themselves as second and third
   arguments. *)

val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b array -> 'c array -> 'a;;
(** [fold_left2 f x a b] computes
   [f (... (f (f x a.(0) b.(0)) a.(1) b.(1)) ...) a.(n - 1) b.(n - 1)],
   where [n] is the length of the arrays [a] and [b].
   
   Raise [Invalid_argument] if the two arrays have different lengths. *)

val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a array -> 'b array -> 'c -> 'c;;
(** [fold_right2 f a b x] computes
   [f a.(0) b.(0) (f a.(1) b.(1) ( ... (f a.(n - 1) b.(n - 1) x) ...))],
   where [n] is the length of the arrays [a] and [b].
   
   Raise [Invalid_argument] if the two arrays have different lengths. *)

val subiter2 :
  ('a -> 'b -> unit) -> 'a array -> 'b array -> int -> int -> int -> unit;;
(** [subiter f a1 a2 o1 o2 len] applies function [f] in turn to the
   elements number [o1] to [o1 + len - 1] of array [a1] and elements number
   [o2] to [o2 + len - 1] of array [a2].
   It is equivalent to
   [f a1.(o1) a2.(o2); f a1.(o1 + 1) a2.(o2 + 1); ...;
   f a1.(o1 + len - 1) a2.(o2 + len - 1); ()].

   Raise [Invalid_argument] if [o1], [o2] and [len] do not designate valid
   subarrays of [a1] and [a2]. *)

val subiteri2 :
  (int -> int -> 'a -> 'b -> unit) -> 'a array -> 'b array -> int -> int -> int
  -> unit;;
(** Same as {!subiter2}, but the function is applied to the index of
   the elements as first and second arguments, and the elements themselves as
   third and fourth arguments. *)

(** {6 Iterators on list of arrays} *)

val maplist : ('a list -> 'a) -> 'a array list -> 'a array;;
(** [maplist f al] applies function [f] to all the elements of the
   items of the array list [al], and builds an array with the results returned
   by [f]:
   [[| f la.(0); f la.(1); ...; f la.(n - 1) |]],
   where [la] is the array of lists [[| [a1.(0); a2.(0); ...; ap.(0)];
   [a1.(1); ...; ap.(1)]; ...;[a1.(n - 1); ...; ap.(n - 1)] |]]
   and [n] is the length of arrays of the list [al].
   
   Raise [Invalid_argument] if the arrays of the list have different lengths. *)

(** {6 Array scanning} *)

val for_all : ('a -> bool) -> 'a array -> bool;;
(** [for_all p [|a1; ...; an|]] checks if all elements of the array
    satisfy the predicate [p]. That is, it returns
    [(p a1) && (p a2) && ... && (p an)]. *)

val exists : ('a -> bool) -> 'a array -> bool;;
(** [exists p [|a1; ...; an|]] checks if at least one element of
   the array satisfies the predicate [p]. That is, it returns
   [(p a1) || (p a2) || ... || (p an)]. *)

val find : ('a -> bool) -> 'a array -> 'a;;
(** [find p [|a1; ...; an|]] returns the first element of the array
   that satisfies the predicat [p] if any, otherwise raise [Not_found]. *) 
