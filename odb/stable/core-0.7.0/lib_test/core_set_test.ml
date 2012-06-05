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

open OUnit
open Core.Std
open Bin_prot.Utils

module StringSet = Set.Make(String)

let s1 = StringSet.of_list ["a"; "b"; "c"; "d"]
(*let m2 = Map.of_alist ["a",1; "c",-3; "d",4; "e",5]*)

type int_set = int Set.t with bin_io

let test = 
  "core_fset" >:::
    [ "sexp" >::
        (fun () -> 
          let s = "(a b c d)" in
          let s1' = StringSet.t_of_sexp (Sexp.of_string s) in
          "of_sexp1" @? (StringSet.equal s1' s1);
          let s_dup = "(a b a d)" in
          let s_dup = Sexp.of_string s_dup in
          assert_raises
            (Sexplib.Conv.Of_sexp_error (
              Failure "Set.t_of_sexp: duplicate element in set",
              (sexp_of_string "a")))
            (fun () -> StringSet.t_of_sexp s_dup)
        );
      "bin_io" >::
        (fun () ->
          let max_n = 20 in
          let bstr = Bigstring.create (max_n + 1) in
          for n = 0 to max_n do
            let s1 = Set.of_array (Array.init n ~f:succ) in
            let pos = bin_write_int_set bstr ~pos:0 s1 in
            "pos" @? (pos = n + 1);
            let pos_ref = ref 0 in
            let s2 = bin_read_int_set bstr ~pos_ref in
            "pos_ref" @? (!pos_ref = n + 1);
            "equal" @? (Set.equal s1 s2);
            bstr.{0} <- '\002';
            bstr.{1} <- 'x';
            bstr.{2} <- 'x';
            pos_ref := 0;
            let dup_check =
              try
                ignore (bin_read_int_set bstr ~pos_ref);
                false
              with Bin_prot.Common.Read_exc (Failure _, 3) -> true
            in
            "dup_check" @? dup_check
          done;
        )
    ]
