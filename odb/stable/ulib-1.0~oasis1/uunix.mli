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

(* $Id: uunix.mli,v 1.1 2007-04-02 17:48:29 weis Exp $ *)

val spawn_init :
    (Scanf.Scanning.scanbuf * out_channel -> 'a) ->
      string -> Scanf.Scanning.scanbuf option ref -> out_channel option ref ->
        Scanf.Scanning.scanbuf * out_channel;;

val spawn :
    string -> Scanf.Scanning.scanbuf option ref -> out_channel option ref ->
      Scanf.Scanning.scanbuf * out_channel;;
(** [spawn cname bin cout] spawns command [cname], if it has not yet been
    launched. More precisely, if the contents of references [bin] or [cout]
    is [None] then:
    - the command [cname] is launched,
    - an input buffer to its [stdin] is assigned to [cin],
    - an output channel to its [stdout] is assigned to [cout].
    Nothing happens if the contents of references [bin] and [cout]
    are not [None]. *)

val launch_connected_processes : string -> string -> unit;;
 (** [launch_connected_processes prog1 prog2] launches [prog1] and
 [prog2] with each other reading and writing to the corresponding
 [stdin/stdout] of the other program. *)
