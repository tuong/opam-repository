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

(* $Id: uprinter.mli,v 1.1 2007-04-02 17:48:29 weis Exp $ *)

type 'a t = Buffer.t -> 'a -> unit;;
type 'a printer = 'a t;;
(** The type of printers on buffers. *)

(** {6 Compact printing of multi-element values} *)

val set_max_iprint : int -> unit;;
(** To set the maximum number of elements to print for compact printing of
   multi-element values. *)

val print_same_elements : int ref -> int ref -> 'a -> 'a t -> 'a ref t;;
val print_same_last_elements : int ref -> int ref -> 'a t -> 'a ref t;;
(** Generic printers for compact printing of multi-element values. *)
