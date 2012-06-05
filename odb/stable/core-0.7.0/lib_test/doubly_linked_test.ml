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
open Doubly_linked

let lists =
  [[];
   [1];
   [1; 2];
   [1; 2; 3];
  ]

let test =
  "doubly_linked" >:::
    [ "empty" >::
        (fun () ->
          let t = create () in
          assert (length t = 0);
          assert (is_empty t);
          assert (first_elt t = None);
          assert (last_elt t = None);
          assert (first t = None);
          assert (last t = None);
          assert (remove_first t = None);
          assert (remove_last t = None);
          assert (to_list t = [])
        );
      "single" >::
        (fun () ->
          let t = create () in
          let elt = insert_first t 13 in
          assert (length t = 1);
          assert (not (is_empty t));
          assert (first t = Some 13);
          assert (last t = Some 13);
          assert (to_list t = [13]);
          assert (is_first t elt);
          assert (is_last t elt);
        );
      "container" >::
        (fun () ->
          let module T = Container_test.Test_S1 (Doubly_linked) in
          T.test ();
        );
      "of_list" >::
        (fun () ->
          for i = 0 to 5 do
            let l = List.init i ~f:ident in
            let t = of_list l in
            assert (l = to_list t);
          done
        );
      "clear" >::
        (fun () ->
          for i = 0 to 5 do
            let t = of_list (List.init i ~f:ident) in
            clear t;
            assert (is_empty t)
          done);
      "transfer" >::
        (fun () ->
          for i1 = 0 to 3 do
            let l1 = List.init i1 ~f:ident in
            for i2 = 0 to 3 do
              let l2 = List.init i2 ~f:ident in
              let t1 = of_list l1 in
              let t2 = of_list l2 in
              transfer ~src:t1 ~dst:t2;
              assert (is_empty t1);
              assert (to_list t2 = l2 @ l1);
            done
          done
        );
      "insert-remove" >::
        (fun () ->
          let t = create () in
          let is_elts elts =
            assert (to_list t = List.map elts ~f:Elt.value);
            let rec loop elt elts =
              match (elt, elts) with
              | (None, []) -> ()
              | (Some elt, elt' :: elts) ->
                  assert (Elt.equal elt elt');
                  loop (next t elt) elts
              | _ -> assert false
            in
            loop (first_elt t) elts;
            begin match elts with
            | [] -> ()
            | elt :: elts ->
                assert (prev t elt = None);
                assert (is_first t elt);
                assert (Option.equal Elt.equal (first_elt t) (Some elt));
                List.iter elts ~f:(fun elt -> assert (not (is_first t elt)));
                ignore
                  (List.fold elts ~init:elt ~f:(fun prev elt ->
                    assert (Option.equal Elt.equal (Doubly_linked.prev t elt)
                               (Some prev));
                    elt));
            end;
            begin match List.rev elts with
            | [] -> ()
            | elt :: elts ->
                assert (next t elt = None);
                assert (is_last t elt);
                assert (Option.equal Elt.equal (last_elt t) (Some elt));
                List.iter elts ~f:(fun elt -> assert (not (is_last t elt)));
                ignore (List.fold elts ~init:elt ~f:(fun next elt ->
                  assert (Option.equal Elt.equal
                             (Doubly_linked.next t elt) (Some next));
                  elt))
            end
          in
          let elt1 = insert_first t () in
          is_elts [elt1];
          let elt2 = insert_first t () in
          is_elts [elt2; elt1];
          let elt3 = insert_last t () in
          is_elts [elt2; elt1; elt3];
          remove t elt1;
          is_elts [elt2; elt3];
          let elt4 = insert_after t elt2 () in
          is_elts [elt2; elt4; elt3];
          let elt5 = insert_before t elt2 () in
          is_elts [elt5; elt2; elt4; elt3];
          ignore (remove_last t);
          is_elts [elt5; elt2; elt4];
          ignore (remove_first t);
          is_elts [elt2; elt4];
          ignore (remove_first t);
          is_elts [elt4];
          ignore (remove_first t);
          is_elts [];
        );
      "filter-inplace" >::
        (fun () ->
          let t = create () in
          let r1 = ref 0 in
          let r2 = ref 1 in
          let r3 = ref 2 in
          let i x = ignore (insert_first t x) in
          i r1;
          i r2;
          i r3;
          assert (length t = 3);
          filter_inplace t ~f:(fun r -> not (phys_equal r r2));
          assert (length t = 2);
          let len =
            fold t ~init:0 ~f:(fun acc x ->
              assert (not (phys_equal x r2));
              acc + 1)
          in
          assert (len = length t))
    ]
