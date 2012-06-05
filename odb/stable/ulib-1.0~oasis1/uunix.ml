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

(* $Id: uunix.ml,v 1.1 2007-04-02 17:48:29 weis Exp $ *)

let spawn_init init_command command bin cout =
  match !bin, !cout with
  | Some ib, Some oc ->
(*       prerr_endline "Spawn_init: command already launched."; *)
      ib, oc
  | Some _, None | None, Some _ ->
      failwith "Spawn_init: Internal error, please report."
  | _ ->
(*       prerr_endline "Spawn_init: launching command."; *)
      let ic, oc = Unix.open_process command in
      let ib = Scanf.Scanning.from_channel ic in
      bin := Some ib; cout := Some oc;
      init_command (ib, oc);
      ib, oc;;

let spawn command = spawn_init ignore command;;

(* Connected processes stuff. *)
let connect_stdio proc (fdin1, fdout1) (fdin2, fdout2) =
  Unix.close fdout1;
  Unix.close fdin2;
  Unix.dup2 fdin1 Unix.stdin;
  Unix.close fdin1;
  Unix.dup2 fdout2 Unix.stdout;
  Unix.close fdout2;
  proc ();;

let connect_bi_directional proc1 proc2 =
  let p1 = Unix.pipe () in
  let p2 = Unix.pipe () in
  match Unix.fork () with
  | 0 -> connect_stdio proc2 p1 p2
  | _ -> connect_stdio proc1 p2 p1;;

let launch prog () = Unix.execv prog [| prog |];;

let launch_connected_processes prog1 prog2 =
  connect_bi_directional (launch prog1) (launch prog2);;
