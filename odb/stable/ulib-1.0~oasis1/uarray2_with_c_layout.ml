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

(* $Id: uarray2_with_c_layout.ml,v 1.2 2008-02-21 17:37:02 fclement Exp $ *)

(* More operations on two-dimensional bigarrays with C layout. *)

open Bigarray;;

let dim1 = Array2.dim1
and dim2 = Array2.dim2
and get = Array2.get
and set = Array2.set;;

(* Iterators. *)

let iter f a =
  let ni = dim1 a in
  let nj = dim2 a in
  for i = 0 to ni - 1 do
    for j = 0 to nj - 1 do
      f (get a i j)
    done;
  done;;

let iterij f a =
  let ni = dim1 a in
  let nj = dim2 a in
  for i = 0 to ni - 1 do
    for j = 0 to nj - 1 do
      f i j (get a i j)
    done;
  done;;

(* Printers. *)

let printer print_elem ob a =
  let ni = dim1 a in
  let print_matrix ob =
    iterij
      (fun i _j aij ->
        Printf.bprintf ob "%a;" print_elem aij;
        if i = ni - 1 then Printf.bprintf ob "\n") in
  Printf.bprintf ob "[|\n%a|]" print_matrix a;;
