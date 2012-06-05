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
open Array

let ar1 = [|1;2;3;4;5;6;7;8;9;10|]

let ( =|= ) list array = list = Array.to_list array

let test =
  "core_array" >:::
    [ "slice" >::
        (fun () ->
           "all" @? (slice ar1 0 0 = ar1);
           "ordinary" @? (slice ar1 1 3 = [|2;3|]);
           "neg1" @? (slice ar1 0 (-1) = [|1;2;3;4;5;6;7;8;9|]);
           "neg2" @? (slice ar1 (-1) 0 = [|10|]);
           "neg3" @? (slice ar1 (-5) (-4) = [|6|];)
        );
      "nget" >::
        (fun () ->
           "neg" @? (nget ar1 (-3) = 8);
           "pos" @? (nget ar1 3 = ar1.(3));
           "invalid" @?
             (try ignore (nget ar1 (-100)); false
              with Invalid_argument _ -> true | _ -> false)
        );
      "filter_opt" >::
        (fun () ->
           "none" @? (filter_opt [|None;None;None|] = [||]);
           "single" @? (filter_opt [|None;Some 3;None|] = [|3|]);
           "singlef" @? (filter_opt [|None;Some 3.;None|] = [|3.|]);
           "double" @? (filter_opt [|None; Some 3; Some 4|] = [|3;4|]);
        );
      "swap" >::
        (fun () ->
           let array = [|0; 1; 2; 3|] in
           "same" @? (swap array 0 0; array = [|0; 1; 2; 3|]);
           "different" @? (swap array 0 3; array = [|3; 1; 2; 0|]);
        );
      "exists" >::
        (fun () ->
           let list =
             List.init ~f:(fun _ -> Random.int 1000) 1000
           in
           let array = Array.of_list list in
           "list1" @? (List.exists ~f:((=) 1) list = Array.exists ~f:((=) 1) array);
           "list2" @? (List.exists ~f:((=) 2) list = Array.exists ~f:((=) 2) array);
           "list3" @? (List.exists ~f:((=) 3) list = Array.exists ~f:((=) 3) array);
           "list4" @? (List.exists ~f:((=) 4) list = Array.exists ~f:((=) 4) array);
        );
      "for_all" >::
        (fun () ->
           let list = Quickcheck.lg (fun () -> Random.int 1000) ~size_gen:(fun _ -> 1000) () in
           let array = Array.of_list list in
           "list1" @? (List.for_all ~f:((<>) 1) list = Array.for_all ~f:((<>) 1) array);
           "list2" @? (List.for_all ~f:((<>) 2) list = Array.for_all ~f:((<>) 2) array);
           "list3" @? (List.for_all ~f:((<>) 3) list = Array.for_all ~f:((<>) 3) array);
           "list4" @? (List.for_all ~f:((<>) 4) list = Array.for_all ~f:((<>) 4) array);
        );
      "mem" >::
        (fun () ->
           let list = Quickcheck.lg (fun () -> Random.int 1000) ~size_gen:(fun _ -> 1000) () in
           let array = Array.of_list list in
           "list1" @? (List.mem 1 ~set:list = Array.mem 1 array);
           "list2" @? (List.mem 2 ~set:list = Array.mem 2 array);
           "list3" @? (List.mem 3 ~set:list = Array.mem 3 array);
           "list4" @? (List.mem 4 ~set:list = Array.mem 4 array);
        );
      "rev" >::
        (fun () ->
           let ordered_list = List.init 100 ~f:(fun i -> i) in
           let empty_list = [] in
           let one_list = [0] in
           "ordered" @?
             (let ordered_array = Array.of_list ordered_list in
              Array.rev_inplace ordered_array;
              List.rev ordered_list =|= ordered_array);
           "empty" @?
             (let empty_array = Array.of_list empty_list in
              Array.rev_inplace empty_array;
              List.rev empty_list =|= empty_array);
           "one" @?
             (let one_array = Array.of_list one_list in
              Array.rev_inplace one_array;
              List.rev one_list =|= one_array);
        );
      "replace_all" >::
        (fun () ->
           let random_list =
             Quickcheck.lg (fun () -> Random.int 1000) ~size_gen:(fun _ -> 1000) ()
           in
           let empty_list = [] in
           let one_list = [0] in
           let f i = i * i in
           "random" @?
             (let random_array = Array.of_list random_list in
              Array.replace_all ~f random_array;
              List.map ~f random_list =|= random_array);
           "empty" @?
             (let empty_array = Array.of_list empty_list in
              Array.replace_all ~f empty_array;
              List.map ~f empty_list =|= empty_array);
           "one" @?
             (let one_array = Array.of_list one_list in
              Array.replace_all ~f one_array;
              List.map ~f one_list =|= one_array);
        );
      "cartesian_product" >::
        (fun () ->
          "empty1" @? is_empty (cartesian_product [||] [||]);
          "empty2" @? is_empty (cartesian_product [||] [|13|]);
          "empty3" @? is_empty (cartesian_product [|13|] [||]);
          "simple" @?
            (cartesian_product [|1; 2; 3;|] [|"a"; "b";|]
              = [|(1, "a"); (1, "b"); (2, "a"); (2, "b"); (3, "a"); (3, "b");|]));
    ]
