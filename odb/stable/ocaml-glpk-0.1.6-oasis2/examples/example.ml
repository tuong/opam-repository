#use "topfind";;
#require "glpk";;

(**
  * Direct translation of the example of glpk's reference manual.
  *
  * @author Samuel Mimram
  *)

(* $Id: example.ml 86 2006-12-18 14:00:12Z smimram $ *)

open Glpk

let () =
  let lp = make_problem Maximize
             [|10.; 6.; 4.|]
             [|
               [|1.; 1.; 1.|];
               [|10.; 4.; 5.|];
               [|2.; 2.; 6.|]
             |]
             [| -.infinity, 100.; -.infinity, 600.; -.infinity, 300. |]
             [| 0., infinity; 0., infinity; 0., infinity|] in
    scale_problem lp;
    use_presolver lp true;
    simplex lp;
    let prim = get_col_primals lp in
      Printf.printf "Z: %g    x0: %g    x1: %g    x2: %g\n%!" (get_obj_val lp) prim.(0) prim.(1) prim.(2)
