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

(** Tests for Filename. *)

open OUnit
open Core.Std
module Filename = Core_extended.Std.Filename

let printer a = a

let test_normalize ~original ~expected () =
  assert_equal ~printer expected (Filename.normalize original)

let test_parent ~original ~expected () =
  assert_equal ~printer expected (Filename.parent original)

let test_make_relative ?to_ ~expected f () =
  let res = try
    Some (Filename.make_relative ?to_ f)
  with Failure _ -> None
  in
  assert_equal ~printer:(Option.value ~default:"none") expected res

let test =
  "filename" >::: [
    "normalize" >::: [
      "id" >::
        test_normalize ~original:"/mnt/local" ~expected:"/mnt/local";
      "dot_dotdot" >::
        test_normalize ~original:"/mnt/./../local" ~expected:"/local";
      "2" >::
        test_normalize
        ~original:"/mnt/local/../global/foo"
        ~expected:"/mnt/global/foo";
      "beyond_root" >::
        test_normalize
        ~original:"/mnt/local/../../.."
        ~expected:"/";
      "negative_lookahead" >::
        test_normalize
        ~original:"../a/../../b"
        ~expected:"../../b"
    ];
    "parent" >::: [
      "1" >:: test_parent ~original:"/mnt/local" ~expected:"/mnt";
      "2" >:: test_parent
        ~original:"/mnt/local/../global/foo" ~expected:"/mnt/global";
      "3" >:: test_parent ~original:"/mnt/local/../../global" ~expected:"/";
    ];
    "make_relative" >::: [
      "1" >:: test_make_relative ~to_:".." "a" ~expected:None;
      "2" >:: test_make_relative ~to_:".." "../a" ~expected:(Some "a");
      "3" >:: test_make_relative ~to_:"c" "a/b" ~expected:(Some "../a/b");
      "4" >:: test_make_relative ~to_:"/" "a/b" ~expected:None;
    ]
  ]
