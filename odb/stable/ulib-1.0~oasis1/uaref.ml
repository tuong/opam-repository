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

(* $Id: uaref.ml,v 1.1 2007-04-02 17:48:29 weis Exp $ *)

(* Abstract reference. *)
let make_ref default =
  let r = ref default in
  (fun () -> !r),
  (fun x -> r := x);;

(* Abstract reference with single assignment. *)
let make_ref_single name default =
  let set = ref false in
  let r = ref default in
  (fun () -> !r),
  (fun x ->
    if not !set then begin r := x; set := true end else
    failwith (Printf.sprintf "Option %s has already been set" name));;

let get_cautious, set_cautious = make_ref true;;
let set_cautious_flag () = set_cautious true
and unset_cautious_flag () = set_cautious false;;

(* Abstract reference with protected assigment. *)
let make_ref_protected name default =
  let set = ref false in
  let r = ref default in
  (fun () -> !r),
  (fun x ->
    if not (!set && get_cautious ()) then begin r := x; set := true end else
    failwith (Printf.sprintf "Option %s has already been set" name));;

(* Abstract reference with mandatory assignment. *)
let make_ref_mandatory name default =
  let set = ref false in
  let r = ref default in
  (fun () ->
    if !set then !r else
    failwith (Printf.sprintf "Option %s has not been set" name)),
  (fun x -> r := x; set := true);;

(* Abstract reference with unique assignment. *)
let make_ref_unique name default =
  let set = ref false in
  let r = ref default in
  (fun () ->
    if !set then !r else
    failwith (Printf.sprintf "Option %s has not been set" name)),
  (fun x ->
    if not !set then begin r := x; set := true end else
    failwith (Printf.sprintf "Option %s has already been set" name));;

(* Abstract reference list. *)
let make_ref_list default =
  let set = ref false in
  let r = ref default in
  (fun () -> if not !set then !r else List.rev !r),
  (fun x ->
    if not !set then begin r := []; set := true end;
    r := x :: !r);;
