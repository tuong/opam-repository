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
  "bag" >:::
    [ "foo" >::
        (fun () ->
          "create" @? begin
            let b = Bag.create () in
            Bag.invariant b;
            assert (Bag.is_empty b);
            assert (0 = Bag.length b);
            true;
          end;
          "add1" @? begin
            let b = Bag.create () in
            let _e1 = Bag.add b 1 in
            Bag.invariant b;
            assert (1 = Bag.length b);
            assert (not (Bag.is_empty b));
            true;
          end;
          "add2" @? begin
            let b = Bag.create () in
            let _e1 = Bag.add b 1 in
            let _e2 = Bag.add b 2 in
            Bag.invariant b;
            assert (2 = Bag.length b);
            assert (not (Bag.is_empty b));
            true
          end;
          "remove" @? begin
            let b = Bag.create () in
            ignore (Bag.remove b (Bag.add b 1));
            Bag.invariant b;
            assert (Bag.is_empty b);
            true;
          end;
          "remove2" @? begin
            let b = Bag.create () in
            let e1 = Bag.add b 1 in
            let _e2 = Bag.add b 2 in
            ignore (Bag.remove b e1);
            Bag.invariant b;
            assert (1 = Bag.length b);
            true;
          end;
          "add100" @? begin
            let b = Bag.create () in
            let n = 20 in
            for i = 1 to n do
              let _e = Bag.add b i in
              Bag.invariant b;
            done;
            assert (Bag.length b = n);
            for i = 1 to n do
              Bag.invariant b;
              match Bag.remove_one b with
              | None -> assert false
              | Some _ -> ()
            done;
            assert (Bag.is_empty b);
            Bag.invariant b;
            true
          end;
          "container" @? begin
            let b = Bag.create () in
            let n = 20 in
            for i = 1 to n do
              ignore (Bag.add b i);
            done;
            assert (n = Bag.fold b ~init:0 ~f:(fun n _ -> n + 1));
            for i = 1 to n do
              assert (Bag.exists b ~f:(fun i' -> i = i'));
            done;
            Bag.iter b ~f:(fun i -> assert (1 <= i && i <= n));
            assert (Bag.for_all b ~f:(fun i -> 1 <= i && i <= n));
            true;
          end;
        )
    ]
