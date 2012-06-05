(* $Id: uarray2.mli,v 1.2 2007-07-13 12:53:27 fclement Exp $ *)

(** More two-dimensional bigarray operations. *)

open Bigarray;;

(** {6 Printers} *)

val printer : 'a Uprinter.t -> ('a, 'b, 'c) Array2.t Uprinter.t;;
(** [printer print ob a] prints the two-dimensional bigarray [a] on
    the output buffer [ob] by evaluating [print ob] on all the elements of [a].
*)

(** {6 Iterators} *)

val iter : ('a -> unit) -> ('a, 'b, 'c) Array2.t -> unit;;
(** [iter f a] applies function [f] in turn to all the elements of the
    bigarray [a]. *)

val iterij : (int -> int -> 'a -> unit) -> ('a, 'b, 'c) Array2.t -> unit;;
(** Same as {!iter}, but the function is applied to the indices
   of the element as first and second arguments, and the element itself as
   third argument. *)
