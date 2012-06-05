(* $Id: uarray2.ml,v 1.1 2007-04-02 17:48:29 weis Exp $ *)

(* More two-dimensional bigarray operations. *)

open Bigarray;;

let dim1 = Array2.dim1
and dim2 = Array2.dim2
and get = Array2.get
and set = Array2.set;;

let bounds a =
  let ni = dim1 a in
  let nj = dim2 a in
  let l = Array1.layout a in
  let ifirst =
    match l with
    | c_layout -> 0
    | fortran_layout -> 1 in
  let ilast = ifirst + ni - 1 in
  let jfirst = ifirst in
  let jlast = jfirst + nj - 1 in
  ifirst, ilast, jfirst, jlast;;

(* Iterators. *)

let iter f a =
  let ifirst, ilast, jfirst, jlast = bounds a in
  for i = ifirst to ilast do
    for j = jfirst to jlast do
      f (get a i j)
    done;
  done;;

let iterij f a =
  let ifirst, ilast, jfirst, jlast = bounds a in
  for i = ifirst to ilast do
    for j = jfirst to jlast do
      f i j (get a i j)
    done;
  done;;

(* Printers. *)

let printer print_elem ob a =
  let _, ilast, _, _ = bounds a in
  let print_matrix ob =
    iterij
      (fun i j aij ->
        Printf.bprintf ob "%a;" print_elem aij;
        if i = ilast then Printf.bprintf ob "\n") in
  Printf.bprintf ob "[|\n%a|]" print_matrix a;;
