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

(* $Id: ublockmatrix.mli,v 1.2 2007-07-13 12:53:27 fclement Exp $ *)

(** Block matrix operations. *)

type 'a t = 'a Umatrix.t Umatrix.t;;
(** The type of block matrices.
   A block matrix is a two-dimensional array of matrices. *)

val size : 'a t -> int * int;;
(** Return the size of the given block matrix.
    The size of a block matrix is a pair of integers that gives the number
    of blocks in both directions. *)

val length : 'a t -> int * int;;
(** [Blockmatrix.length] is an alias for {!Blockmatrix.size}. *)

(*val sizes : 'a t -> int array * int array;;
(** Return the sizes (pair of arrays of numbers of elements in both directions)
   of the given block matrix. *)

val lengths : 'a t -> int array * int array;;
(** [Blockmatrix.lengths] is an alias for {!Blockmatrix.sizes}. *)

val to_matrix : 'a t -> 'a Umatrix.t;;
(** *)

val of_matrix : 'a Umatrix.t -> int array -> int array -> 'a t;;
(** *)
*)
val resize : int -> int -> 'a Umatrix.t -> 'a Umatrix.t;;
(** [Blockmatrix.resize ni nj m] returns a fresh matrix of size [(ni, nj)].
   Each element of the given matrix [m] becomes a submatrix of size

   Raise [Invalid_arg] if the matrix [m] has a zero dimension and not... . *)

val zoom : int -> int -> 'a t -> 'a t;;
(** [Blockmatrix.zoom ni nj m] returns a fresh matrix whose dimensions are
   those of [m] multiplied by [ni] and [nj]. Each element of the given matrix
   [m] becomes a submatrix of size [(ni, nj)]. *)

