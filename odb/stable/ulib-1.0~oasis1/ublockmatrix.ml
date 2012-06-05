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

(* $Id: ublockmatrix.ml,v 1.2 2007-07-13 12:53:27 fclement Exp $ *)

(** Block matrix operations. *)

type 'a t = 'a Umatrix.t Umatrix.t;;

let size = Umatrix.size;;

let length = size;;

(*let sizes bm =
  let ni1, nj1 = size bm in
  
  ;;

let lengths = sizes;;

let to_matrix bm =
  let ni1, nj1 = Umatrix.size bm in
  if ni1 = 0 || nj1 = 0 then [||] else
  
  let ni2, nj2 = in

  let ni = and nj = in
  let m = make ni nj bm.(0).(0).(0).(0) in
  Umatrix.iterijij (fun i j bmij -> ) bm;;

  ;;

let of_matrix m is js =
  ;;
*)

let resize ni2 nj2 m1 =
  let ni1, nj1 = length m1 in
  match ni1, nj1, ni2, nj2 with
  | 0, _, 0, _ -> [||]
  | 0, _, _, _ -> invalid_arg "Ublockmatrix.resize"
  | _, 0, _, 0 -> Array.make ni2 m1.(0)
  | _, 0, _, _ -> invalid_arg "Ublockmatrix.resize"
  | _, _, _, _ -> 
      let m2 = Umatrix.make ni2 nj2 m1.(0).(0) in
      let di = float_of_int ni2 /. float_of_int ni1
      and dj = float_of_int nj2 /. float_of_int nj1 in
      let f d eps i = int_of_float ((float_of_int i +. eps) *. d) in
      let fdim = f di 0. and fdip i = (f di 1. i) - 1
      and fdjm = f dj 0. and fdjp j = (f dj 1. j) - 1 in
      Umatrix.iterij
        (fun i1 j1 x ->
          for i2 = max 0 (fdim i1) to min (fdip i1) (ni2 - 1) do
            for j2 = max 0 (fdjm j1) to min (fdjp j1) (nj2 - 1) do
              m2.(i2).(j2) <- x
            done;
          done)
        m1;
      m2;;

let zoom ni nj m =
  let ni1, nj1 = length m in
  resize (ni * ni1) (nj * nj1) m;;

