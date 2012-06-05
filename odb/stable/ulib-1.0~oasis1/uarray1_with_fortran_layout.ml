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

(* $Id: uarray1_with_fortran_layout.ml,v 1.2 2007-07-13 12:53:27 fclement Exp $ *)

(* More operations on one-dimensional bigarrays with Fortran layout. *)

open Bigarray;;
open Uprinter;;

let length = Array1.dim
and get = Array1.get
and set = Array1.set;;

let init kind n f =
  let a = Array1.create kind fortran_layout n in
  for i = 1 to n do set a i (f i) done;
  a;;

let make kind n x =
  let a = Array1.create kind fortran_layout n in
  Array1.fill a x;
  a;;

(* Iterators. *)

let iter f a =
  let n = length a in
  for i = 1 to n do f (get a i) done;;

let map f a =
  let n = length a in
  let b = Array1.create (Array1.kind a) fortran_layout n in
  for i = 1 to n do set b i (f (get a i)) done;
  b;;

let fold_left f x a =
  let n = length a in
  let r = ref x in
  for i = 1 to n do r := f !r (get a i) done;
  !r;;

(* Printers. *)

let column_printer print_elem ob a =
  let print_array ob = iter (Printf.bprintf ob "%a;\n" print_elem) in
  Printf.bprintf ob "[|\n%a|]" print_array a;;

let line_printer print_elem ob a =
  let print_array ob = iter (Printf.bprintf ob "%a; " print_elem) in
  Printf.bprintf ob "[|%a|]" print_array a;;

let compact_printer print_elem ob a =
  let print_array ob a = 
    if length a > 0 then
      let count = ref 0
      and iprint = ref 0
      and prev = ref (get a 1) in
      let f ai = print_same_elements iprint count ai print_elem ob prev in
      iter f a;
      print_same_last_elements iprint count print_elem ob prev in
  Printf.bprintf ob "[|%a|]" print_array a;;

(* Iterators on two bigarrays. *)

let same_length s a1 a2 =
  let n = length a1 in
  if n = length a2 then n else
  invalid_arg ("Uarray1_with_fortran_layout." ^ s);;

let fold_left2 f x a1 a2 =
  let n = same_length "fold_left2" a1 a2 in
  let r = ref x in
  for i = 1 to n do r := f !r (get a1 i) (get a2 i) done;
  !r;;
