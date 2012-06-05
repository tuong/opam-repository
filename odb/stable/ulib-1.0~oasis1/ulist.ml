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

(* $Id: ulist.ml,v 1.3 2009-06-22 09:11:29 fclement Exp $ *)

(** Local definitions to be removed if inserted into the official List module *)

let rev_append = List.rev_append;;
let fold_left = List.fold_left;;

(** More list operations. *)

let printer print_elem ob l =
  let print_list ob = List.iter (Printf.bprintf ob "%a; " print_elem) in
  Printf.bprintf ob "[%a]" print_list l;;

let replace_by_list i l1 l2 =
  let rec loop i2 = function
    | [] -> []
    | x :: l ->
        if i2 <> i then x :: loop (i2 + 1) l else
        l1 @ loop (i2 + 1) l in
  loop 0 l2;;

let rev_replace_by_list i l1 l2 =
  let rec loop accu i2 = function
    | [] -> accu
    | x :: l ->
        loop
          (if i2 <> i then x :: accu else rev_append l1 accu)
          (i2 + 1) l in
  loop [] 0 l2;;

let insert i l1 l2 =
  let rec loop i2 = function
    | [] -> []
    | x :: l ->
        if i2 <> i then x :: loop (i2 + 1) l else
        (l1 @ [x]) @ loop (i2 + 1) l in
  loop 0 l2;;

let rev_insert i l1 l2 =
  let rec loop accu i2 = function
    | [] -> accu
    | x :: l -> 
        loop
          (if i2 <> i then x :: accu else x :: rev_append l1 accu)
          (i2 + 1) l in
  loop [] 0 l2;;

(** {6 Iterators} *)

let map_i f l =
  let rec loop i = function
  | [] -> []
  | x :: l -> f i x :: loop (i + 1) l in
  loop 0 l;;

let rev_map_i f l =
  let rec loop accu i = function
    | [] -> accu
    | x :: l -> loop (f i x :: accu) (i + 1) l in
  loop [] 0 l;;

let replace i0 x0 l = map_i (fun i x -> if i <> i0 then x else x0) l;;

let rev_replace i0 x0 l = rev_map_i (fun i x -> if i <> i0 then x else x0) l;;

let fold_left_i f accu l =
  let rec loop i accu = function
  | [] -> accu
  | x :: l -> loop (i + 1) (f i accu x) l in
  loop 0 accu l
;;

(** {6 Association lists} *)

let min_assoc cmp = function
  | [] -> invalid_arg "Ulist.min_assoc"
  | x0 :: l ->
      fold_left
        (fun (y0, _ as x0) (y, _ as x) ->
          if cmp y0 y > 0 then x else x0)
        x0 l;;

let max_assoc cmp = function
  | [] -> invalid_arg "Ulist.max_assoc"
  | x0 :: l ->
      fold_left
        (fun (y0, _ as x0) (y, _ as x) ->
          if cmp y0 y < 0 then x else x0)
        x0 l;;

let min_fst_assoc = min_assoc;;
let max_fst_assoc = max_assoc;;

let min_snd_assoc cmp = function
  | [] -> invalid_arg "Ulist.min_snd_assoc"
  | x0 :: l ->
      fold_left
        (fun (_, y0 as x0) (_, y as x) ->
          if cmp y0 y > 0 then x else x0)
        x0 l;;

let max_snd_assoc cmp = function
  | [] -> invalid_arg "Ulist.max_snd_assoc"
  | x0 :: l ->
      fold_left
        (fun (_, y0 as x0) (_, y as x) ->
          if cmp y0 y < 0 then x else x0)
        x0 l;;

let rec rank e = function
| [] -> raise Not_found
| x :: l -> if x = e then 0 else 1 + rank e l;;
