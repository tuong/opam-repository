(******************************************************************************
 *                             Core-extended                                  *
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
open Core_extended.Std
open OUnit

type tests = {
  good_pos : Sexp.t list;
  bad_pos : Sexp.t list;
  good_pos0 : Sexp.t list;
  bad_pos0 : Sexp.t list;
  good_neg : Sexp.t list;
  bad_neg : Sexp.t list;
  good_neg0 : Sexp.t list;
  bad_neg0 : Sexp.t list;
  good_bound : Sexp.t list;
  bad_bound : Sexp.t list;
} with sexp

module type Spec = sig
  include Number.Spec
  include Number.Verified_std with type repr = t
end

let tests =
  Memo.unit (fun () ->
    Sexp.load_sexp_conv_exn "numbers_test.sexp" tests_of_sexp)

module Make_test (Spec : Spec) = struct
  open Spec

  let test_good name lst t_of_sexp =
    List.iter lst ~f:(fun sexp ->
      try ignore (t_of_sexp sexp)
      with _ ->
        failwithf "%s failed on good: %s" name
          (Spec.to_string (Spec.t_of_sexp sexp)) ())

  let test_bad name lst t_of_sexp =
    List.iter lst ~f:(fun sexp ->
      try
        ignore (t_of_sexp sexp);
        failwithf "%s failed on bad: %s" name
          (Spec.to_string (Spec.t_of_sexp sexp)) ()
      with Of_sexp_error _ -> ())

  let test name good bad t_of_sexp =
    test_good name good t_of_sexp;
    test_bad name bad t_of_sexp

  let () =
    let tests = tests () in
    test "Pos" tests.good_pos tests.bad_pos Pos.t_of_sexp;
    test "Pos0" tests.good_pos0 tests.bad_pos0 Pos0.t_of_sexp;
    test "Neg" tests.good_neg tests.bad_neg Neg.t_of_sexp;
    test "Neg0" tests.good_neg0 tests.bad_neg0 Neg0.t_of_sexp;
    let module Bounded_spec =
      struct
        let name = "Bound"
        let lower = of_string "3"
        let upper = of_string "42"
      end
    in
    let module Bounded = Spec.Make_bounded (Bounded_spec) in
    test "Bounded" tests.good_bound tests.bad_bound Bounded.t_of_sexp;
end

let test =
  "Numbers_test" >:::
    [
      "Int" >:: (fun () ->
        let module My_test = Make_test (Int) in
        ());
      "Int32" >:: (fun () ->
        let module My_test = Make_test (Int32) in
        ());
      "Int63" >:: (fun () ->
        let module My_test = Make_test (Int63) in
        ());
      "Int64" >:: (fun () ->
        let module My_test = Make_test (Int64) in
        ());
      "Nativeint" >:: (fun () ->
        let module My_test = Make_test (Nativeint) in
        ());
      "Float" >:: (fun () ->
        let module My_test = Make_test (Float) in
        ());
    ]
