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

(* $Id: ustring.mli,v 1.1 2007-04-02 17:48:29 weis Exp $ *)

(** More string operations. *)

val init : int -> (int -> char) -> string;;
(** [init n f] returns a fresh string of length [n],
   with element number [i] initialized to the result of [f i].
   In other terms, [init n f] tabulates the results of [f]
   applied to the integers [0] to [n-1].

   Raise [Invalid_argument] if [n < 0] or [n > Sys.max_string_length].*)
