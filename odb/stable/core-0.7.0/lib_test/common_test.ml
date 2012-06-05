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
  "common" >:::
    [ "% and /%" >::
        (fun () ->
           let gen_int_pair () = (Quickcheck.uig (), abs (Quickcheck.uig ())) in
           let modulus_invariant (a, b) =
             let r = a % b in
             let q = a /% b in
             r >= 0 && a = q * b + r
           in
           Quickcheck.laws_exn "modulus invariant"
             1000 gen_int_pair modulus_invariant
        );

      "memoize" >::
        (fun () ->
           let f x = x * x in
           let memo_f = Memo.general f in
           Quickcheck.laws_exn "memoize"
             1000 Quickcheck.uig (fun i -> f i = memo_f i)
        );

      "nan" >::
        (fun () ->
          let nan = 0. /. 0. in
          "fmin1" @? (Float.is_nan (Float.min 1. nan));
          "fmin2" @? (Float.is_nan (Float.min nan 0.));
          "fmin3" @? (Float.is_nan (Float.min nan nan));
          "fmax1" @? (Float.is_nan (Float.max 1. nan));
          "fmax2" @? (Float.is_nan (Float.max nan 0.));
          "fmax3" @? (Float.is_nan (Float.max nan nan));
          "fmin_inan1" @? (1. = (Float.min_inan 1. nan));
          "fmin_inan2" @? (0. = (Float.min_inan nan 0.));
          "fmin_inan3" @? (Float.is_nan (Float.min_inan nan nan));
          "fmax_inan1" @? (1. = (Float.max_inan 1. nan));
          "fmax_inan2" @? (0. = (Float.max_inan nan 0.));
          "fmax_inan3" @? (Float.is_nan (Float.max_inan nan nan));
        );

      "round" >::
        (fun () ->
          "zero" @? (Float.iround_exn 0.2 = 0);
          "negative zero" @? (Float.iround_exn (-0.2) = 0);
          "positive" @? (Float.iround_exn 3.4 = 3);
          "negative" @? (Float.iround_exn (-3.4) = -3);
        );

    ]


