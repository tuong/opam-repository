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
open Quickcheck


let dup gen () = let x = gen () in x,x

let true_count l =
  List.fold l
    ~f:(fun acc v -> if v then acc + 1 else acc)
    ~init:0

let definitive_clause l = true_count l = 1

let unescaped_test ~name s = name @? (String.unescaped (String.escaped s) = s)

let test =
  "extended_string" >:::
  [
    "collate" >::
      (fun () ->
         let (<!) s s' = String.collate s s' < 0
         and (>!) s s' = String.collate s s' > 0
         in
         let basic_tests = (fun (s,s') ->
                      "invertible" @? ((s' <! s) = (s >! s'));
                      "total" @? (definitive_clause [s<!s'; s=s'; s>!s']))
         in
         repeat 50 basic_tests (pg sg sg);
         repeat 2 basic_tests (dup sg);
         repeat 50 (fun (s,s',s'') ->
                      let (s1,s2,s3) =
                        match List.sort ~cmp:String.collate [s;s';s''] with
                        | [s1;s2;s3] -> s1,s2,s3
                        | _ -> assert false
                      in
                      "transitive" @?
                        (((s1 <! s2) || (s2 <! s3)) = (s1 <! s3)))
           (tg sg sg sg);
         "a2b <! a10b" @? ("a2b" <! "a10b");
         "a2b <! a02b" @? ("a2b" <! "a02b");
         "a010b <! a20b" @? ("a010b" <! "a20b");
      );
      "unescaped" >::
        (fun () ->
           unescaped_test ~name:"empty" "";
           repeat 50 (unescaped_test ~name:"random") sg;
           "hex" @? (String.unescaped "\\xff" = "\xff");
           "strict illegal escape" @?
             (try ignore (String.unescaped "\\a"); false
              with Invalid_argument _ -> true);
           "non strict" @? (String.unescaped ~strict:false "\\a" = "\\a");
           "non-strict illegal escape" @?
             (try ignore (String.unescaped ~strict:false "\\512"); false
              with Invalid_argument _ -> true)
        );
  ]
