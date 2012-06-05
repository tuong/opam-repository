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

module type T = sig
  type 'a container
  val fold : 'a container -> init:'b -> f:('b -> 'a -> 'b) -> 'b
end

module Make (T:T) = struct
  include T

  let iter t ~f = fold t ~init:() ~f:(fun () a -> f a)

  let length c = fold c ~init:0 ~f:(fun acc _ -> acc + 1)

  let is_empty c =
    with_return (fun r ->
      iter c ~f:(fun _ -> r.return false);
      true)

  let exists c ~f =
    with_return (fun r ->
      iter c ~f:(fun x -> if f x then r.return true);
      false)

  let for_all c ~f =
    with_return (fun r ->
      iter c ~f:(fun x -> if not (f x) then r.return false);
      true)

  let find c ~f =
    with_return (fun r ->
      iter c ~f:(fun x -> if f x then r.return (Some x));
      None)

  let to_list c =
    List.rev (fold c ~init:[] ~f:(fun acc x -> x :: acc))

  let to_array c = Array.of_list (to_list c)

  let container = {
    Container.
    iter     = iter;
    fold     = fold;
    length   = length;
    is_empty = is_empty;
    exists   = exists;
    for_all  = for_all;
    find     = find;
    to_list  = to_list;
    to_array = to_array;
  }
end

