(* $Id: uarray1.ml,v 1.2 2007-07-13 12:53:27 fclement Exp $ *)

(* More one-dimensional bigarray operations. *)

open Bigarray;;
open Uprinter;;

let length = Array1.dim
and get = Array1.get
and set = Array1.set;;

let first l =
  if l = c_layout then 0 else
  if l = fortran_layout then 1 else
  assert false;;

let bounds a =
  let n = length a in
  let l = Array1.layout a in
  let ifirst = first l in
  let ilast = ifirst + n - 1 in
  ifirst, ilast;;

let init n f =
  let a = Array1.create kind layout n in
  let ifirst, ilast = bounds a in
  for i = ifirst to ilast do set a i (f i) done;
  a;;

let make kind layout n x =
  let a = Array1.create kind layout n in
  Array1.fill a x;
  a;;

(* Iterators. *)

let iter f a =
  let ifirst, ilast = bounds a in
  for i = ifirst to ilast do f (get a i) done;;

let map f a =
  let n = length a in
  let b = Array1.create (Array1.kind a) (Array1.layout a) n in
  let ifirst, ilast = bounds a in
  for i = ifirst to ilast do set b i (f (get a i)) done;
  b;;

let fold_left f x a =
  let ifirst, ilast = bounds a in
  let r = ref x in
  for i = ifirst to ilast do r := f !r (get a i) done;
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
      let ifirst, _ = bounds a in
      let count = ref 0
      and iprint = ref 0
      and prev = ref (get a ifirst) in
      let f ai = print_same_elements iprint count ai print_elem ob prev in
      iter f a;
      print_same_last_elements iprint count print_elem ob prev in
  Printf.bprintf ob "[|%a|]" print_array a;;

(* Iterators on two bigarrays. *)

let fold_left2 f x a1 a2 =
  let n1 = length a1 in
  let n2 = length a2 in
  if n1 <> n2 then invalid_arg "Uarray1.fold_left2" else
  let ifirst, ilast = bounds a1 in
  let r = ref x in
  for i = ifirst to ilast do r := f !r (get a1 i) (get a2 i) done;
  !r;;
