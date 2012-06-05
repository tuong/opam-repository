(******************************************************************************
 *                             Core                                           *
 *                                                                            *
 * Copyright (C) 2008- Jane Street Holding, LLC                               *
 *    Contact: opensource@janestreet.com                                      *
 *    WWW: http://www.janestreet.com/ocaml                                    *
 *                                                                            *
 *                                                                            *
 * This library is free software; you can redistribute it and/or              *
 * modify it under the terms of the GNU Lesser General Public                 *
 * License as published by the Free Software Foundation; either               *
 * version 2 of the License, or (at your option) any later version.           *
 *                                                                            *
 * This library is distributed in the hope that it will be useful,            *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 * Lesser General Public License for more details.                            *
 *                                                                            *
 * You should have received a copy of the GNU Lesser General Public           *
 * License along with this library; if not, write to the Free Software        *
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                            *
 ******************************************************************************)

open Core.Std
open OUnit

let () = Random.self_init ()

(* round:
   ...  <-)[-><-)[-><-)[-><-)[-><-)[-><-)[->   ...
   ... -+-----+-----+-----+-----+-----+-----+- ...
   ... -3    -2    -1     0     1     2     3  ...
   so round x -. x should be in (-0.5,0.5]
*)
let round_test x =
  let y = Float.round x in
  -0.5 < y -. x && y -. x <= 0.5

(* Float.iround built so this should always be true *)
let iround_test x =
  let y = Float.iround x in
  match y with
  | None -> true
  | Some y ->
    let y = float_of_int y in
    -0.5 < y -. x && y -. x <= 0.5

(* round_down:
   ... )[<---)[<---)[<---)[<---)[<---)[<---)[  ...
   ... -+-----+-----+-----+-----+-----+-----+- ...
   ... -3    -2    -1     0     1     2     3  ...
   so x -. round_down x should be in [0,1)
*)
let round_down_test x =
  let y = Float.round_down x in
  match y with
  | None -> true
  | Some y ->
    let y = float_of_int y in
    0. <= x -. y && x -. y < 1.

(* round_up:
   ...  ](--->](--->](--->](--->](--->](--->]( ...
   ... -+-----+-----+-----+-----+-----+-----+- ...
   ... -3    -2    -1     0     1     2     3  ...
   so round_up x -. x should be in [0,1)
*)
let round_up_test x =
  let y = Float.round_up x in
  match y with
  | None -> true
  | Some y ->
    let y = float_of_int y in
    0. <= y -. x && y -. x < 1.

(* round_towards_zero:
   ...  ](--->](--->](---><--->)[<---)[<---)[  ...
   ... -+-----+-----+-----+-----+-----+-----+- ...
   ... -3    -2    -1     0     1     2     3  ...
   so abs x -. abs (round_towards_zero x) should be in [0,1)
*)
let round_towards_zero_test x =
  let y = Float.round_towards_zero x in
  match y with
  | None -> true
  | Some y ->
    let x = Float.abs x in
    let y = Float.abs (float_of_int y) in
    0. <= x -. y && x -. y < 1.

let special_values_test () =
  Float.round (-.1.50001) = -.2. &&
  Float.round (-.1.5) = -.1. &&
  Float.round (-.0.50001) = -.1. &&
  Float.round (-.0.5) = 0. &&
  Float.round 0.49999 = 0. &&
  Float.round 0.5 = 1. &&
  Float.round 1.49999 = 1. &&
  Float.round 1.5 = 2. &&
  Float.round_up_exn (-.2.) = -2 &&
  Float.round_up_exn (-.1.9999) = -1 &&
  Float.round_up_exn (-.1.) = -1 &&
  Float.round_up_exn (-.0.9999) = 0 &&
  Float.round_up_exn 0. = 0 &&
  Float.round_up_exn 0.00001 = 1 &&
  Float.round_up_exn 1. = 1 &&
  Float.round_up_exn 1.00001 = 2 &&
  Float.round_down_exn (-.1.00001) = -2 &&
  Float.round_down_exn (-.1.) = -1 &&
  Float.round_down_exn (-.0.00001) = -1 &&
  Float.round_down_exn 0. = 0 &&
  Float.round_down_exn 0.99999 = 0 &&
  Float.round_down_exn 1. = 1 &&
  Float.round_down_exn 1.99999 = 1 &&
  Float.round_down_exn 2. = 2 &&
  Float.round_towards_zero_exn (-.2.) = -2 &&
  Float.round_towards_zero_exn (-.1.99999) = -1 &&
  Float.round_towards_zero_exn (-.1.) = -1 &&
  Float.round_towards_zero_exn (-.0.99999) = 0 &&
  Float.round_towards_zero_exn 0.99999 = 0 &&
  Float.round_towards_zero_exn 1. = 1 &&
  Float.round_towards_zero_exn 1.99999 = 1 &&
  Float.round_towards_zero_exn 2. = 2

(* code for generating random floats on which to test functions *)
let absirand () =
  let rec aux acc cnt =
    if cnt = 0 then
      acc
    else
      let bit = if Random.bool () then 1 else 0 in
      aux (2 * acc + bit) (cnt / 2)
  in
  (* we only go to 2. ** 52. -. 1. since our various round functions lose precision after
     this and our tests may (and do) fail when precision is lost *)
  aux 0 (int_of_float (2. ** 52. -. 1.))

let frand () =
  let x = (float (absirand ())) +. Random.float 1.0 in
  if Random.bool () then
    -1.0 *. x
  else
    x

let test =
  let randoms = List.init ~f:(fun _ -> frand ()) 10_000 in
  "core_float" >:::
    [ "to_string_hum" >::
        (fun () ->
          "random iround" @? (randoms |! List.for_all ~f:iround_test);
          "max_int iround" @? iround_test (float_of_int max_int);
          "min_int iround" @? iround_test (float_of_int min_int);
          "random round" @? (randoms |! List.for_all ~f:round_test);
          "random round_towards_zero_exn" @?
            (randoms |! List.for_all ~f:round_towards_zero_test);
          "random round_down_exn" @? (randoms |! List.for_all ~f:round_down_test);
          "random round_up_exn" @? (randoms |! List.for_all ~f:round_up_test);
          "special values" @? special_values_test ();
        )
    ]
