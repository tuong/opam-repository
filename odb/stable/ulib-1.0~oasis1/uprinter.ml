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

(* $Id: uprinter.ml,v 1.2 2007-07-13 12:53:27 fclement Exp $ *)

type 'a t = Buffer.t -> 'a -> unit;;
type 'a printer = 'a t;;

(* Compact printing of multi-element values *)

let get_max_iprint, set_max_iprint = Uaref.make_ref 25;;

let print_same_elements iprint count a print_elem ob prev =
  let max_iprint = get_max_iprint () in
  if !iprint = max_iprint then begin
    Printf.bprintf ob "...";
    incr iprint end else begin
  if !iprint < max_iprint then begin
    if a = !prev then incr count else begin
    if !count > 1 then Printf.bprintf ob "(%dx) " !count;
    Printf.bprintf ob "%a; " print_elem !prev;
    count := 1;
    incr iprint;
    prev := a end end end;;

let print_same_last_elements iprint count print_elem ob prev =
  let max_iprint = get_max_iprint () in
  if !iprint = max_iprint then Printf.bprintf ob "..." else begin
  if !iprint < max_iprint then begin
    if !count > 1 then Printf.bprintf ob "(%dx) " !count;
    Printf.bprintf ob "%a" print_elem !prev end end;;
