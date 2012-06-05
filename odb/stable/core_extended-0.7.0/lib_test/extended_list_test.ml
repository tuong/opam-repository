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
open OUnit

module L = Core_extended.Std.List

let is_even x = x mod 2 = 0

let test = "Extended_list" >::: [
  "number" >::
    (fun () ->
       "base" @? (L.number [1;2;3;1;4] = [1,0;2,0;3,0;1,1;4,0]));
  "multimerge" >::
    (fun () ->
       "base" @? (L.multimerge [[0;2];[2;3];[0;1];[1;2]] = [0;1;2;3]);
       "dup" @? (L.multimerge [[0;1;2;0];[0;1]] = [0;1;2;0]);
       (* There is no solution here: we just want to make sure that the
          result has all the fields. *)
       "circle" @? (
         let header = L.multimerge [[0;1;2];[0;2;1;4]] in
         List.sort ~cmp:Int.compare header = [0;1;2;4]));
  ("take_while" >:: fun () ->
    "take evens" @? (
      (L.take_while [2;4;6;7;8;9] is_even) = [2;4;6]));
  ("equal" >:::
    let equal xs ys = L.equal ~equal:Int.equal xs ys in
    let assert_equal xs ys = assert (equal xs ys) in
    let assert_not_equal xs ys = assert (not (equal xs ys)) in
  [
    ("1" >:: fun () -> assert_equal     []     []);
    ("2" >:: fun () -> assert_not_equal [2]    []);
    ("3" >:: fun () -> assert_not_equal []     [3]);
    ("4" >:: fun () -> assert_equal     [4]    [4]);
    ("5" >:: fun () -> assert_not_equal [0; 5] [0]);
    ("6" >:: fun () -> assert_not_equal [0]    [0; 6]);
    ("7" >:: fun () -> assert_equal     [0; 7] [0; 7]);
  ]);
  ("compare" >:::
    let compare xs ys = L.compare ~cmp:Int.compare xs ys in
    let assert_eq xs ys = assert (compare xs ys = 0) in
    let assert_lt xs ys = assert (compare xs ys < 0) in
    let assert_gt xs ys = assert (compare xs ys > 0) in
  [
    ("1" >:: fun () -> assert_eq  []     []);
    ("2" >:: fun () -> assert_gt  [2]    []);
    ("3" >:: fun () -> assert_lt  []     [3]);
    ("4" >:: fun () -> assert_eq  [4]    [4]);
    ("4" >:: fun () -> assert_lt  [3]    [4]);
    ("4" >:: fun () -> assert_gt  [3]    [2]);
    ("5" >:: fun () -> assert_gt  [0; 5] [0]);
    ("6" >:: fun () -> assert_lt  [0]    [0; 6]);
    ("5" >:: fun () -> assert_lt  [0; 5] [1]);
    ("6" >:: fun () -> assert_gt  [1]    [0; 6]);
    ("7" >:: fun () -> assert_eq  [0; 7] [0; 7]);
  ]);
  ("transpose" >:::
    let assert_eq a b = assert (a = b) in
    let round_trip a b =
      assert_eq (L.transpose a) (Some b);
      assert_eq (L.transpose b) (Some a)
    in
    [
      ("0x0" >:: fun () -> round_trip [] []);
      ("1x0" >:: fun () -> assert_eq (L.transpose [[]]) (Some []));
      ("2x0" >:: fun () -> assert_eq (L.transpose [[];[]]) (Some []));
      ("3x0" >:: fun () -> assert_eq (L.transpose [[];[];[]]) (Some []));
      ("1x1" >:: fun () -> round_trip [[1]] [[1]]);
      ("1x2" >:: fun () -> round_trip [[1];
                                       [2]] [[1;2]]);
      ("1x3" >:: fun () -> round_trip [[1];
                                       [2];
                                       [3]] [[1;2;3]]);
      ("2x2" >:: fun () -> round_trip [[1;2];
                                       [3;4]] [[1;3];
                                               [2;4]]);
      ("2x3" >:: fun () -> round_trip [[1;2;3];
                                       [4;5;6]] [[1;4];
                                                 [2;5];
                                                 [3;6]]);
      ("no1" >:: fun () -> assert_eq (L.transpose [[];[1]]) None);
      ("no2" >:: fun () -> assert_eq (L.transpose [[1;2];[3]]) None);
    ]);
]
