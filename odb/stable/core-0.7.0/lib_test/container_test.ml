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

open Core.Std

module Test_S1
  (M : sig
    include Container.S1
    val of_list : 'a list -> 'a container
  end) : sig
    val test : unit -> unit
end = struct
  let lists = List.init 10 ~f:(fun i -> List.init i ~f:ident)
      
  let test () =
    List.iter lists ~f:(fun l ->
      let m = M.of_list l in
      assert (M.length m = List.length l);
      assert (M.is_empty m = List.is_empty l);
      let is_l_unsorted l' = l = List.sort l' ~cmp:Int.compare in
      assert (is_l_unsorted (M.fold m ~init:[] ~f:(fun ac x -> x :: ac)));
      assert (is_l_unsorted (M.to_list m));
      let r = ref [] in
      M.iter m ~f:(fun x -> r := x :: !r);
      assert (is_l_unsorted !r);
      assert (is_l_unsorted (Array.to_list (M.to_array m)));
      List.iter l ~f:(fun x -> assert (M.exists m ~f:(fun x' -> x = x')));
      assert (M.for_all m ~f:(fun x -> List.exists l ~f:(fun x' -> x = x')));
      List.iter l ~f:(fun x -> assert (Some x = M.find m ~f:(fun x' -> x = x')));
    )
  ;;
end
