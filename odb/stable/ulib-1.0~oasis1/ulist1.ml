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

(* $Id: ulist1.ml,v 1.3 2009-12-12 17:40:15 fclement Exp $ *)

type 'a list1 = 'a * 'a list;;

type 'a t = 'a list1;;

let length (_, l) = 1 + List.length l;;

let to_list (h, l) = h :: l;;

let of_list = function
| [] -> invalid_arg "List1.of_list"
| h :: l -> h, l;;

let concat ll = of_list (List.concat (to_list ll));;

let replace_by_list i l1 l2 =
  of_list (Ulist.replace_by_list i (to_list l1) (to_list l2));;

let map f (h, l) = (f h, List.map f l);;

let map_i f (h, l) = (f 0 h, Ulist.map_i (fun i -> f (i + 1)) l);;

let iter f (h, l) = f h; List.iter f l;;

let fold_left f accu (h, l) = List.fold_left f (f accu h) l;;

let fold_right f (h, l) accu = f h (List.fold_right f l accu);;

let map2 f (h1, l1) (h2, l2) = (f h1 h2, List.map2 f l1 l2);;

let iter2 f (h1, l1) (h2, l2) = f h1 h2; List.iter2 f l1 l2;;

let find p (h, l) = if p h then h else List.find p l;;

let find_all p (h, l) =
  if p h then h :: List.find_all p l else List.find_all p l;;

let filter = find_all;;

let partition p l =
  let l1, l2 = List.partition p (to_list l) in
  of_list l1, of_list l2;;

let max_snd_assoc cmp (x0, l) =
  List.fold_left
    (fun (_, y0 as x0) (_, y as x) ->
      if cmp y0 y < 0 then x else x0)
    x0 l;;
