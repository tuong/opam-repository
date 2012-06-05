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

(* $Id: umatrix.ml,v 1.4 2008-02-21 17:37:02 fclement Exp $ *)

(** Matrix operations. *)

type 'a t = 'a array array;;
type 'a matrix = 'a t;;

let size m =
  let ni = Array.length m in
  if ni = 0 then 0, 0 else begin
    let nj = Array.length (Array.unsafe_get m 0) in
    for i = 1 to ni - 1 do
      if Array.length (Array.unsafe_get m i) <> nj
      then invalid_arg "Umatrix.size"
    done;
    ni, nj
  end;;

let length = size;;

let unsafe_size m =
  let ni = Array.length m in
  if ni = 0 then 0, 0 else
  let nj = Array.length (Array.unsafe_get m 0) in
  ni, nj;;

let dim_i = Array.length;;

let dim_j m = snd (size m);;

let unsafe_dim_j m = snd (unsafe_size m);;

let dim1 = dim_i
and dim2 = dim_j;;

let get m i = Array.get (Array.get m i)
and set m i = Array.set (Array.get m i);;

let unsafe_get m i = Array.unsafe_get (Array.unsafe_get m i)
and unsafe_set m i = Array.unsafe_set (Array.unsafe_get m i);;

let init ni nj f =
  if ni < 0 || nj < 0 then invalid_arg "Umatrix.init" else
  Array.init ni (fun i -> Array.init nj (f i));;

let make ni nj x =
  if ni < 0 || nj < 0 then invalid_arg "Umatrix.make" else
  Array.init ni (fun _i -> Array.make nj x);;

(*let make ni nj x = init ni nj (fun i j -> x);;*)

let printer print_elem ob m =
  let print_matrix ob =
    Array.iter (Printf.bprintf ob "%a;\n" (Uarray.line_printer print_elem)) in
  Printf.bprintf ob "[|\n%a|]" print_matrix m;;

let compact_printer print_elem ob m =
  let print_matrix ob =
    Array.iter
      (Printf.bprintf ob "%a;\n" (Uarray.compact_printer print_elem)) in
  Printf.bprintf ob "[|\n%a|]" print_matrix m;;

let check s f m1 m2 =
  if f m1 <> f m2 then invalid_arg ("Umatrix." ^ s) else ();;
(** [check s f m1 m2] raises [Invalid_arg] if [f] applied to matrices
   [m1] and [m2] do not return the same result. *)

let append_i m1 m2 =
  check "append_i" dim_j m1 m2;
  Array.append m1 m2;;

let append_j m1 m2 =
  check "append_j" dim_i m1 m2;
  Uarray.map2 Array.append m1 m2;;

let append1 = append_i
and append2 = append_j;;

let append m1 m2 m3 m4 = append_i (append_j m1 m2) (append_j m3 m4);;

let list_check s f = function
  | [] -> ()
  | x :: l -> List.iter (fun y -> if f y <> f x then invalid_arg s) l;;
(** [list_check s f ml] raises [Invalid_arg] if [f] applied to all items
   of the matrix list [ml] do not return the same result. *)

let concat_i ml =
  list_check "concat_i" dim_j ml;
  Array.concat ml;;

let concat_j ml =
  list_check "concat_j" dim_i ml;
  Uarray.maplist Array.concat ml;;

let concat1 = concat_i
and concat2 = concat_j;;

let concat mll = concat_i (List.map concat_j mll);;

let sub m oi li oj lj =
  let ni, nj = size m in
  if oi < 0 || li < 0 || oi + li > ni ||
     oj < 0 || lj < 0 || oj + lj > nj
  then invalid_arg "Umatrix.sub"
  else init li lj (fun i j -> unsafe_get m (oi + i) (oj + j));;

let copy m =
  let ni, nj = size m in
  init ni nj (fun i j -> unsafe_get m i j);;

let fill m oi li oj lj x =
  let ni, nj = length m in
  if oi < 0 || li < 0 || oi + li > ni ||
     oj < 0 || lj < 0 || oj + lj > nj
  then invalid_arg "Umatrix.fill"
  else
    for i = oi to oi + li - 1 do
      for j = oj to oj + lj - 1 do
        unsafe_set m i j x
      done;
    done;;

let blit m1 oi1 oj1 m2 oi2 oj2 li lj =
  let ni1, nj1 = size m1 and ni2, nj2 = size m2 in
  if li < 0 || lj < 0 ||
     oi1 < 0 || oi1 + li > ni1 || oj1 < 0 || oj1 + lj > nj1 ||
     oi2 < 0 || oi2 + li > ni2 || oj2 < 0 || oj2 + lj > nj2
  then invalid_arg "Umatrix.blit"
  else
    let bl i j =
      unsafe_set m2 (oi2 + i) (oj2 + j) (unsafe_get m1 (oi1 + i) (oj1 + j)) in
     if oi1 < oi2 then
       for i = li - 1 downto 0 do
         if oj1 < oj2 then for j = lj - 1 downto 0 do bl i j done else
         for j = 0 to lj - 1 do bl i j done
       done
     else
       for i = 0 to li - 1 do
         if oj1 < oj2 then for j = lj - 1 downto 0 do bl i j done else
         for j = 0 to lj - 1 do bl i j done
       done;;

let transpose m =
  let ni, nj = size m in
  if ni == 0 || nj == 0 then [||] else
  let mt = make nj ni (unsafe_get m 0 0) in
  for j = 0 to nj - 1 do
    for i = 0 to ni - 1 do
      unsafe_set mt j i (unsafe_get m i j)
    done;
  done;
  mt;;

let to_array m = Array.concat (Array.to_list m);;

let of_array a ni nj =
  let len = Array.length a in
  if len <> ni * nj then invalid_arg "Umatrix.of_array" else
  init ni nj (fun i j -> Array.unsafe_get a (nj * i + j));;

let to_arraylist = Array.to_list;;

let of_arraylist = function
  | [] -> [||]
  | a :: _ as al ->
      let ni = Array.length a in
      List.iter
        (fun b ->
          if Array.length b <> ni then invalid_arg "Umatrix.of_arraylist")
        al;
      Array.of_list al;;

let to_list m = List.map Array.to_list (Array.to_list m);;

let of_list l = Array.map Array.of_list (Array.of_list l);;

(** {6 Iterators} *)

let iter f =
  Array.iter (fun mi -> Array.iter f mi);;

let map f =
  Array.map (fun mi -> Array.map f mi);;

let iterij f =
  Array.iteri (fun i mi -> Array.iteri (fun j x -> f i j x) mi);;

let mapij f =
  Array.mapi (fun i mi -> Array.mapi (fun j x -> f i j x) mi);;

let iter_i = Array.iter;;

let iter_j f m = iter_i f (transpose m);;

let map_i = Array.map;;

let map_j f m = transpose (map_i f (transpose m));;

let fold_left f =
  Array.fold_left (fun x mi -> Array.fold_left f x mi);;

let fold_right f =
  Array.fold_right (fun mi x -> Array.fold_right f mi x);;

let subiter f m oi li oj lj =
  Uarray.subiter (fun mi -> Uarray.subiter f mi oj lj) m oi li;;

let subiterij f m oi li oj lj =
  Uarray.subiteri (fun i mi -> Uarray.subiteri (f i) mi oj lj) m oi li;;

(** {6 Iterators on two matrices} *)

let iter2 f =
  Uarray.iter2 (fun m1i m2i -> Uarray.iter2 f m1i m2i);;

let map2 f =
  Uarray.map2 (fun m1i m2i -> Uarray.map2 f m1i m2i);;

let iterij2 f =
  Uarray.iteri2 (fun i m1i m2i -> Uarray.iteri2 (f i) m1i m2i);;

let mapij2 f =
  Uarray.mapi2 (fun i m1i m2i -> Uarray.mapi2 (f i) m1i m2i);;

let fold_left2 f =
  Uarray.fold_left2 (fun x m1i m2i -> Uarray.fold_left2 f x m1i m2i);;

let fold_right2 f =
  Uarray.fold_right2 (fun m1i m2i x -> Uarray.fold_right2 f m1i m2i x);;

(** {6 Specific applications} *)

let make_diagonal_matrix zero v =
  let n = Array.length v in
  let f i j = if i = j then v.(i) else zero in
  init n n f;;

let make_int_diagonal_matrix v = make_diagonal_matrix 0 v;;
let make_float_diagonal_matrix v = make_diagonal_matrix 0. v;;

let resize ni2 nj2 m1 =
  let ni1, nj1 = length m1 in
  match ni1, nj1, ni2, nj2 with
  | 0, _, 0, _ -> [||]
  | 0, _, _, _ -> invalid_arg "Umatrix.resize"
  | _, 0, _, 0 -> Array.make ni2 m1.(0)
  | _, 0, _, _ -> invalid_arg "Umatrix.resize"
  | _, _, _, _ -> 
      let m2 = make ni2 nj2 m1.(0).(0) in
      let di = float_of_int ni2 /. float_of_int ni1
      and dj = float_of_int nj2 /. float_of_int nj1 in
      let f d eps i = int_of_float ((float_of_int i +. eps) *. d) in
      let fdim = f di 0. and fdip i = (f di 1. i) - 1
      and fdjm = f dj 0. and fdjp j = (f dj 1. j) - 1 in
      iterij
        (fun i1 j1 x ->
          for i2 = max 0 (fdim i1)  to min (fdip i1) (ni2 - 1) do
            for j2 = max 0 (fdjm j1) to min (fdjp j1) (nj2 - 1) do
              m2.(i2).(j2) <- x
            done;
          done)
        m1;
      m2;;

let zoom ni nj m =
  let ni1, nj1 = length m in
  resize (ni * ni1) (nj * nj1) m;;
