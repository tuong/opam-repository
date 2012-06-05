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

let test = 
  "interval" >:::
    [ "is_empty_or_singleton" >:: 
        (fun () -> 
          let t x = Interval.is_empty_or_singleton x in
          let i = Interval.create in
          "singleton1" @? t (i 0 0);
          "singleton2" @? t (i 10 10);
          "singleton3" @? t (i "foo" "foo");
          "empty1" @? t (i 1 0);
          "nonempty" @? not (t (i 0 1));
        );

      "are_disjoint_as_open_intervals" >:: 
        (fun () -> 
          let t x = Interval.are_disjoint_as_open_intervals x in
          let i = Interval.create in
          "touching" @? t [i 3 4; i 4 5];
          "not touching" @? t [i 3 4; i 5 6];
          "overlapping" @? not (t [i 3 5; i 4 6]);
        );

      "contains_set" >::
        (fun () ->
          let module S = Interval.Set in
          let s1 = S.create [ 1,2; 3,4; 5,6 ] in
          let s2 = S.create [ 3,5; 10,11 ] in
          let s3 = S.create [ 3,4 ] in
          "contains 1" @? (S.contains s2 3);
          "contains 2" @? (S.contains s2 4);
          "contains 3" @? (not (S.contains s2 9));
          "contains 4" @? (not (S.contains s2 12));
          "contains_set 1" @? (not (S.contains_set ~container:s2 ~contained:s1));
          "contains_set 2" @? (not (S.contains_set ~container:s1 ~contained:s2));
          "contains_set 3" @? (S.contains_set ~container:s1 ~contained:s3);
          "contains_set 4" @? (S.contains_set ~container:s2 ~contained:s3);
        );

    ]

