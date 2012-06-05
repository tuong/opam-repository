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

(* $Id: uarray.ml,v 1.2 2007-07-13 12:53:27 fclement Exp $ *)

open Uprinter;;

(* More array operations. *)

let length = Array.length;;
let unsafe_get = Array.unsafe_get;;
let unsafe_set = Array.unsafe_set;;
let make = Array.make;;

let array_set_sub f a ofs len =
  if ofs < 0 || len < 0 || ofs + len > length a
  then invalid_arg "Uarray.set_array"
  else for i = ofs to len - 1 do a.(i) <- f a.(i) done;;

let array_set f a = array_set_sub f a 0 (length a);;

let array_seti_sub f a ofs len =
  if ofs < 0 || len < 0 || ofs + len > length a
  then invalid_arg "Uarray.seti_array"
  else for i = ofs to len - 1 do a.(i) <- f i a.(i) done;;

let array_seti f a = array_seti_sub f a 0 (length a);;

(* Printers. *)

let column_printer print_elem ob a =
  let print_array ob = Array.iter (Printf.bprintf ob "%a;\n" print_elem) in
  Printf.bprintf ob "[|\n%a|]" print_array a;;

let line_printer print_elem ob a =
  let print_array ob = Array.iter (Printf.bprintf ob "%a; " print_elem) in
  Printf.bprintf ob "[|%a|]" print_array a;;

let compact_printer print_elem ob a =
  let print_array ob a = 
    if length a > 0 then
      let count = ref 0
      and iprint = ref 0
      and prev = ref (unsafe_get a 0) in
      let f ai = print_same_elements iprint count ai print_elem ob prev in
      Array.iter f a;
      print_same_last_elements iprint count print_elem ob prev in
  Printf.bprintf ob "[|%a|]" print_array a;;

let list_of_list_array la = Array.to_list (Array.map Array.of_list la);;

let list_to_list_array al = Array.of_list (List.map Array.to_list al);;

(* Iterators. *)

let subiter f a ofs len =
  if ofs < 0 || len < 0 || ofs + len > length a
  then invalid_arg "Uarray.subiter"
  else for i = 0 to len - 1 do f (unsafe_get a (ofs + i)) done;;

let subiteri f a ofs len =
  if ofs < 0 || len < 0 || ofs + len > length a
  then invalid_arg "Uarray.subiteri"
  else for i = ofs to ofs + len - 1 do f i (unsafe_get a i) done;;

(* Iterators on two arrays. *)

let same_length s a1 a2 =
  let l = length a1 in
  if l = length a2 then l else
  invalid_arg ("Uarray." ^ s);;

let iter2 f a1 a2 =
  let l = same_length "iter2" a1 a2 in
  for i = 0 to l - 1 do f (unsafe_get a1 i) (unsafe_get a2 i) done;;

let map2 f a1 a2 =
  let l = same_length "map2" a1 a2 in
  if l = 0 then [||] else begin
    let r = make l (f (unsafe_get a1 0) (unsafe_get a2 0)) in
    for i = 1 to l - 1 do
      unsafe_set r i (f (unsafe_get a1 i) (unsafe_get a2 i))
    done;
    r
  end;;

let iteri2 f a1 a2 =
  let l = same_length "iteri2" a1 a2 in
  for i = 0 to l - 1 do f i (unsafe_get a1 i) (unsafe_get a2 i) done;;

let mapi2 f a1 a2 =
  let l = same_length "mapi2" a1 a2 in
  if l = 0 then [||] else begin
    let r = make l (f 0 (unsafe_get a1 0) (unsafe_get a2 0)) in
    for i = 1 to l - 1 do
      unsafe_set r i (f i (unsafe_get a1 i) (unsafe_get a2 i))
    done;
    r
  end;;

let fold_left2 f x a1 a2 =
  let l = same_length "fold_left2" a1 a2 in
  let r = ref x in
  for i = 0 to l - 1 do
    r := f !r (unsafe_get a1 i) (unsafe_get a2 i)
  done;
  !r;;

let fold_right2 f a1 a2 x =
  let l = same_length "fold_right2" a1 a2 in
  let r = ref x in
  for i = l - 1 downto 0 do
    r := f (unsafe_get a1 i) (unsafe_get a2 i) !r done;
  !r;;

let subiter2 f a1 a2 o1 o2 len =
  if len < 0 ||
    o1 < 0 || o1 + len > length a1 ||
    o2 < 0 || o2 + len > length a2
  then invalid_arg "Uarray.subiter2"
  else for i = 0 to len - 1 do
    f (unsafe_get a1 (o1 + i)) (unsafe_get a2 (o2 + i))
  done;;

let subiteri2 f a1 a2 o1 o2 len =
   if len < 0 ||
    o1 < 0 || o1 + len > length a1 ||
    o2 < 0 || o2 + len > length a2
  then invalid_arg "Uarray.subiteri2"
  else for i = 0 to len - 1 do
    f (o1 + i) (o2 + i) (unsafe_get a1 (o1 + i)) (unsafe_get a2 (o2 + i))
  done;;

(* Iterators on list of arrays. *)

let maplist f al = Array.map f (list_to_list_array al);;

(* Array scanning. *)

let for_all p a =
  let len = length a in
  let rec loop i = i >= len || p a.(i) && loop (i + 1) in
  loop 0;;

let exists p a =
  let len = length a in
  let rec loop i = i < len && (p a.(i) || loop (i + 1)) in
  loop 0;;

let find p a =
  let len = length a in
  let rec loop i =
    if i >= len then raise Not_found else
    if p a.(i) then a.(i) else loop (i + 1) in
  loop 0;;
