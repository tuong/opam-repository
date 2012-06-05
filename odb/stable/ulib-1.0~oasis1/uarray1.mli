(* $Id: uarray1.mli,v 1.2 2007-07-13 12:53:27 fclement Exp $ *)

(** More one-dimensional bigarray operations. *)

open Bigarray;;

val init :
    ('a, 'b) kind -> 'c layout -> int -> (int -> 'a) -> ('a, 'b, 'c) Array1.t;;
(** [init kind layout dim f] returns a fresh one-dimensional
    bigarray of size [dim] and layout [layout] whose element number [i] is of
    kind [kind] and is initialized to [f i]. *)

val make : ('a, 'b) kind -> 'c layout -> int -> 'a -> ('a, 'b, 'c) Array1.t;;
(** [make kind layout dim value] returns a fresh one-dimensional
    bigarray of size [dim] and layout [layout] whose elements are of kind
    [kind] and are initialized to [value]. *)

(** {6 Printers} *)

val column_printer : 'a Uprinter.t -> ('a, 'b, 'c) Array1.t Uprinter.t;;
(** [column_printer print ob a] prints the one-dimensional bigarray
    [a] on the output buffer [ob] as a column by evaluating [print ob] on all
    elements of the bigarray. *)

val line_printer : 'a Uprinter.t -> ('a, 'b, 'c) Array1.t Uprinter.t;;
(** Same as {!column_printer} but with line output. *)

val compact_printer : 'a Uprinter.t -> ('a, 'b, 'c) Array1.t Uprinter.t;;
(** Same as {!line_printer} but with compact output. *)

(** {6 Iterators} *)

val iter : ('a -> unit) -> ('a, 'b, 'c) Array1.t -> unit;;
(** [iter f a] applies function [f] in turn to all the elements of
    the bigarray [a]. *)

val map : ('a -> 'a) -> ('a, 'b, 'c) Array1.t -> ('a, 'b, 'c) Array1.t;;
(** [map f a] applies function [f] to all the elements of the bigarray
    [a] and returns a fresh bigarray with the results given by [f]. *)

val fold_left : ('a -> 'b -> 'a) -> 'a -> ('b, 'c, 'e) Array1.t -> 'a;;
(** [fold_left f x a] computes [x(i+1) = f xi a.{i}] over all elements
    of the bigarray [a] with [xfirst = x], and returns [x(last+1)]. *)

(** {6 Iterators on two bigarrays} *)

val fold_left2 :
    ('a -> 'b -> 'c -> 'a) -> 'a -> ('b, 'd, 'e) Array1.t ->
      ('c, 'f, 'g) Array1.t -> 'a;;
(** [fold_left2 f x a b] computes [x(i+1) = f xi a.{i} b.{i}] over all
    elements of the bigarrays [a] and [b] with [xfirst = x], and returns
    [x(last+1)].
    
    Raise [Invalid_argument] if the two bigarrays have different lengths. *)
