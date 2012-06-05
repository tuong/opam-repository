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

let test =
  "extended_float" >:::
    [ "pretty" >:: (fun () ->
        List.iter ~f:(fun (f, s) -> string_of_float f @? (s = Float.pretty ~on_negative:`Print_dir f))
          [ (0.004, "0");
            (0.0049, "0");
            (0.005, "0.01");
            (0.009, "0.01");
            (0.01,  "0.01");
            (0.10,  "0.1");
            (0.99,  "0.99");
            (0.994, "0.99");
            (0.995, "1");
            (1.0,   "1");
            (1.001, "1");
            (1.006, "1.01");
            (1.01,  "1.01");
            (9.99,  "9.99");
            (9.994, "9.99");
            (9.995, "10");
            (9.996, "10");
            (10.0,  "10");
            (10.1,  "10.1");
            (99.9,  "99.9");
            (99.94, "99.9");
            (99.95, "100");
            (99.96, "100");
            (100.0, "100");
            (100.6, "101");
            (999.0, "999");
            (999.4, "999");
            (999.5, "1000");
            (1000.0, "1000");
            (9999.0, "9999");

            (10_000.0, "10k");
            (12_345.0, "12k3");
            (12_500.0, "12k5");
            (99_999.0, "100k");
            (100_000.0, "100k");
            (999_499.0, "999k");

            (999_500.0, "1m");
            (1_000_000.0, "1m");
            (1_230_000.0, "1m23");
            (999_499_000.0, "999m");
          ]);
      "to_float_hum" >:: (fun () ->
        List.iter ~f:(fun (f, s) ->
          string_of_float f @? (s = Float.to_string_hum f))
          [ 1.00004e12, "1.000_04e+12";
            -10004.0004,"-10_004.000_4";
            -14.,"-14.";
          ]);

      "order_of_magnitude_difference" >:: (fun () ->
        let test (a, b, n) =
          (sprintf "((oom_diff %g %g = %d))" a b n)
            @? (Float.order_of_magnitude_difference a b = n)
        in
        List.iter ~f:test [
          (  11.0, 1001.0,   2);
          (1001.0,   11.0,   2);
          ( 131.0,   11.0,   1);
          ( 9.5,     9.0,    0);
          ( 9.5,     1.0,    1);
          ( 200.0,    0.003, 5);
        ]);
    ]
