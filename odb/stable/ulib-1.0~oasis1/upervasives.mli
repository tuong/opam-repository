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

(* $Id: upervasives.mli,v 1.3 2011-01-28 14:18:48 fclement Exp $ *)

(** Extensions to the initially opened module. *)


(** {6 Integer arithmetic} *)

val split : int -> int -> int list;;
(** [split n k] returns a list of [k] integers that differs at most by 1
   and such that their sum equals [n].
   With [k < 0], [split n k] returns [split (-n) (-k)]. *)

val psplit : int -> int -> int list;;
(** [psplit n k] returns a list of at most [k] positive integers that differs
   at most by 1 and such that their sum equals [n].
   With [k < 0], [psplit n k] returns [psplit (-n) (-k)]. *)


(** {6 Floating-point arithmetic} *)

val round : float -> int;;
(** Round the given float to the nearest integer.
   For a nonnegative float [f], [round f] returns {!Pervasives.int_of_float}
   [(f +. 0.5)], ie the greatest integer less than or equal to [f +. 0.5];
   and for a negative float [f], [round f] returns {!Pervasives.int_of_float}
   [(f -. 0.5)], ie the least integer greater than or equal to [f -. 0.5]. *)


(** {6 Conversion} *)

val bit_string_of_float : float -> string;;
(** [bit_string_of_float f] returns a [string] containing the string of bits
   of the binary representation of the float [f]. *)

val float_of_bit_string : string -> float;;
(** [float_of_bit_string] is the converse function of [bit_string_of_float]. *)


(** {6 Input/output} *)

val open_append : string -> out_channel;;
val open_append_bin : string -> out_channel;;
