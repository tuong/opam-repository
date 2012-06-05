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

(* $Id: uaref.mli,v 1.1 2007-04-02 17:48:29 weis Exp $ *)

(** Abstract references.
   Useful to define command line options. *)

val make_ref : 'a -> (unit -> 'a) * ('a -> unit);;
(** Abstract reference.
   [make_ref default] returns a couple of functions, the second one sets a
   variable with a default value [default], and the first one gets its value. *)

val make_ref_single : string -> 'a -> (unit -> 'a) * ('a -> unit);;
(** Abstract reference with single assignment.
   Same as [make_ref] but the variable can be set only once. The first argument
   is the name of the variable. *)

val make_ref_protected : string -> 'a -> (unit -> 'a) * ('a -> unit);;
(** Abstract reference with protected assignment.
   Same as [make_ref_single] but the protection for single assignment only holds
   when the cautious flag is on. *)

val set_cautious_flag : unit -> unit;;
val unset_cautious_flag : unit -> unit;;
(** To set or unset the cautious flag for driving the assignment of protected
   references. Default is on. *)

val make_ref_mandatory : string -> 'a -> (unit -> 'a) * ('a -> unit);;
(** Abstract reference with mandatory assignment.
   Same as [make_ref_single] but the variable has to be set at least once.
   The default value [default] is used for initialization. *)

val make_ref_unique : string -> 'a -> (unit -> 'a) * ('a -> unit);;
(** Abstract reference with unique assignment.
   Same as [make_ref_mandatory] but the variable has to be set exactly once. *)

val make_ref_list : 'a list -> (unit -> 'a list) * ('a -> unit);;
(** Abstract reference list.
   [make_ref_list default_list] returns a couple of functions, the second one
   adds an element to an initially empty list, and the first one gets the list,
   which is the default list [default_list] if no element has been added. *)
