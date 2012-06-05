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

(* $Id: uarray1_with_c_layout.mli,v 1.2 2007-07-13 12:53:27 fclement Exp $ *)

(** More operations on one-dimensional bigarrays with C layout. *)

open Bigarray;;

val init : ('a, 'b) kind -> int -> (int -> 'a) -> ('a, 'b, c_layout) Array1.t;;
(** [init kind dim f] returns a fresh one-dimensional
    bigarray of size [dim] whose element number [i] is of kind [kind] and is
    initialized to [f i]. *)

val make : ('a, 'b) kind -> int -> 'a -> ('a, 'b, c_layout) Array1.t;;
(** [make kind dim value] returns a fresh
    one-dimensional bigarray of size [dim] whose elements are of kind  [kind]
    and are initialized to [value]. *)

(** {6 Printers} *)

val column_printer : 'a Uprinter.t -> ('a, 'b, c_layout) Array1.t Uprinter.t;;
(** [column_printer print ob a] prints the bigarray [a]
    on the output buffer [ob] as a column by evaluating [print ob] on all
    elements of the bigarray. *)

val line_printer : 'a Uprinter.t -> ('a, 'b, c_layout) Array1.t Uprinter.t;;
(** Same as {!column_printer} but with line output. *)

val compact_printer :
  'a Uprinter.t -> ('a, 'b, c_layout) Array1.t Uprinter.t;;
(** Same as {!line_printer} but with compact output. *)

(** {6 Iterators} *)

val iter : ('a -> unit) -> ('a, 'b, c_layout) Array1.t -> unit;;
(** [iter f a] applies function [f] in turn to all the
    elements of the bigarray [a]. *)

val map :
    ('a -> 'a) -> ('a, 'b, c_layout) Array1.t -> ('a, 'b, c_layout) Array1.t;;
(** [map f a] applies function [f] to all the elements
    of the bigarray [a] and returns a fresh bigarray with the results given by
    [f]. *)

val fold_left : ('a -> 'b -> 'a) -> 'a -> ('b, 'c, c_layout) Array1.t -> 'a;;
(** [fold_left f a ] computes 
    [f (... (f (f x a.{0}) a.{1}) ...) a.{n - 1}],
    where [n] is the length of the bigarray [a]. *)

(** {6 Iterators on two bigarrays} *)

val fold_left2 :
    ('a -> 'b -> 'c -> 'a) -> 'a -> ('b, 'd, c_layout) Array1.t ->
      ('c, 'e, c_layout) Array1.t -> 'a;;
(** [fold_left2 f a b] computes 
    [f (... (f (f x a.{0} b.{0}) a.{1} b.{1}) ...) a.{n - 1} b.{n - 1}],
    where [n] is the length of the bigarrays [a] and [b].
    
    Raise [Invalid_argument] if the two bigarrays have different lengths. *)
