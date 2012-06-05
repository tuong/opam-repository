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

(* $Id: uarray2_with_fortran_layout.mli,v 1.2 2007-07-13 12:53:27 fclement Exp $ *)

(** More operations on two-dimensional bigarrays with Fortran layout. *)

open Bigarray;;

(** {6 Printers} *)

val printer : 'a Uprinter.t -> ('a, 'b, fortran_layout) Array2.t Uprinter.t;;
(** [printer print ob a] prints the
    two-dimensional bigarray [a] on the output buffer [ob] by evaluating
    [print ob] on all the elements of [a]. *)

(** {6 Iterators} *)

val iter : ('a -> unit) -> ('a, 'b, fortran_layout) Array2.t -> unit;;
(** [iter f a] applies function [f] in turn to all
    the elements of the bigarray [a]. *)

val iterij :
    (int -> int -> 'a -> unit) -> ('a, 'b, fortran_layout) Array2.t -> unit;;
(** Same as {!iter}, but the function is applied
    to the indices of the element as first and second arguments, and the element
    itself as third argument. *)
