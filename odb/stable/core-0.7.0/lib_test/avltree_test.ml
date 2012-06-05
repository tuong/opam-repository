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

module Avltree = Core.Avltree
open OUnit
open Core.Std

let test_f t s test_data f =
  let is_present i =
    assert (Avltree.mem !t ~compare i);
    begin match Avltree.find !t ~compare i with
    | None -> assert false
    | Some j -> assert (i = j)
    end;
  in
  Avltree.invariant !t ~compare;
  Array.iter test_data ~f:(fun i ->
    t := f ~set:s ~t ~present:(Set.mem !s i) ~i;
    Avltree.invariant !t ~compare;
    Set.iter !s ~f:is_present;
    Avltree.iter !t ~f:(fun ~key ~data -> assert (Set.mem !s key && key = data));
    Avltree.fold !t ~init:() ~f:(fun ~key ~data () ->
      assert (Set.mem !s key && key = data)))
;;

let do_add ~set ~t ~present ~i ~added =
  added := present;
  let res = Avltree.add !t ~compare ~added ~key:i ~data:i in
  if present
  then assert (not !added)
  else assert (!added);
  set := Set.add !set i;
  res
;;

let test_add t s test_data =
  let added = ref false in
  test_f t s test_data (fun ~set ~t ~present ~i ->
    do_add ~set ~t ~present ~i ~added)
;;

let do_remove ~set ~t ~present ~i ~removed =
  removed := not present;
  let res = Avltree.remove !t ~compare ~removed i in
  if present
  then assert (!removed)
  else assert (not !removed);
  set := Set.remove !set i;
  res
;;

let test_remove t s test_data =
  let removed = ref false in
  test_f t s test_data (fun ~set ~t ~present ~i ->
    do_remove ~set ~t ~present ~i ~removed)
;;

let test_add_remove t s test_data =
  let add = ref false in
  let r = ref false in
  test_f t s test_data (fun ~set ~t ~present ~i ->
    if !add
    then (add := false; do_add ~set ~t ~present ~i ~added:r)
    else (add := true; do_remove ~set ~t ~present ~i ~removed:r))
;;

let test name dset add_remove_dset =
  let t = ref Avltree.empty in
  let s = ref Set.empty in
  [ ("add_lookup_" ^ name) >:: (fun () -> test_add t s dset);
    ("add_remove_lookup_" ^ name) >:: (fun () -> test_add_remove t s add_remove_dset);
    ("remove_lookup_" ^ name) >:: (fun () -> test_remove t s dset) ]
;;

let test =
  let size = 1000 in
  let (random_data1, random_data2) =
    let s =
      Random.State.make
        [| Unix.getpid ();
           Option.value_exn
             (Float.round_towards_zero (Time.to_float (Time.now ()))) |]
    in
    Array.init size ~f:(fun _ -> Random.State.int s 10000),
    Array.init size ~f:(fun _ -> Random.State.int s 10000)
  in
  let sorted_data = Array.init size ~f:ident in
  let reverse_sorted_data =
    let x = Array.copy sorted_data in
    Array.rev_inplace x;
    x
  in
  "Avltree" >:::
    (test "random" random_data1 random_data2
     @ test "sorted" sorted_data random_data2
     @ test "reverse_sorted" reverse_sorted_data random_data2)
;;
