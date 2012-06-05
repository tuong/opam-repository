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

open OUnit;;
open Core.Std

let concat_test p1 p2 res =
  (sprintf "%s ^/  %s" p1 p2) @?
    (if p1 ^/ p2 = res then
       true
     else begin
       eprintf "%s ^/ %s = %s (expected %s)"
         p1 p2 (p1 ^/ p2) res;
       false
     end)

let test =
  "core_filename" >:::
    [ "concat" >:: fun () ->
        (List.iter ~f:(fun (p1,p2,expected) -> concat_test p1 p2 expected)
           ["a/","/b","a/b";
            "a","./b","a/b";
            "a",".","a/.";
            "a","/","a/";
            "a/.","./","a/";
            "a///././/./.",".///././/././/b","a/b"
           ]
        )
    ]

