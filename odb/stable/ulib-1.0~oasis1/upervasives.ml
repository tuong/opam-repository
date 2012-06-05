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

(* $Id: upervasives.ml,v 1.4 2011-01-28 14:18:48 fclement Exp $ *)

(** Extensions to the initially opened module. *)

(** {6 Integer arithmetic} *)

(* let (//) a b = int_of_float (ceil (float_of_int a /. float_of_int b));; *)
let (//) a b =
  let q = a / b in
  if a * b <= 0 || a = b * q then q else q + 1;;

let split n k =
  let rec loop accu s nn kk =
    if kk < 0 then loop accu s (-nn) (-kk) else
    if s = abs k then accu else
    if nn = 0 then loop (0 :: accu) (s + 1) nn kk else
    (* It is not necessary to test the nullity of k, *)
    (* since k = 0 && n != 0 is impossible! *)
    (* Indeed, k is positive and decrease by one at each loop step, *)
    (* and as soon as k = 1, then q = n // 1 = n and we loop with n = 0. *)
    let q = nn // kk in
    loop (q :: accu) (s + 1) (nn - q) (kk - 1) in
  loop [] 0 n k;;

let psplit n k = List.filter (fun i -> i > 0) (split n k);;

(** {6 Floating-point arithmetic} *)

let round_pos x = int_of_float (x +. 0.5);;
let round_neg x = int_of_float (x -. 0.5);;
let round x = if x >= 0. then round_pos x else round_neg x;;

(** {6 Conversion} *)

let char_of_int64 i = char_of_int (Int64.to_int (Int64.logand i 0xffL));;

let bit_string_of_float f =
  let i = Int64.bits_of_float f in
  Ustring.init 8 (fun j -> char_of_int64 (Int64.shift_right i (8*j)));;

let int64_of_char c = Int64.of_int (int_of_char c);;

let float_of_bit_string s =
  assert (String.length s = 8);
  let rec loop accu j =
    if j < 0 then accu else
    loop (Int64.add (Int64.shift_left accu 8) (int64_of_char s.[j])) (j - 1) in
  Int64.float_of_bits (loop (int64_of_char s.[7]) 6);;

(** {6 Input/output} *)

let open_append fname =
  let mode = [Open_wronly; Open_append; Open_creat; Open_text;] in
  let perm = 0o644 in
  open_out_gen mode perm fname;;

let open_append_bin fname =
  let mode = [Open_wronly; Open_append; Open_creat; Open_binary;] in
  let perm = 0o644 in
  open_out_gen mode perm fname;;
