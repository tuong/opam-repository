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

open Sexplib.Conv

open Common

module Array = Core_array
module List = Core_list
module Queue = Caml.Queue

exception Empty = Queue.Empty

type 'a t = 'a Queue.t

let create = Queue.create

let enqueue t x = Queue.push x t

let is_empty = Queue.is_empty

let dequeue t = if is_empty t then None else Some (Queue.pop t)

let dequeue_exn = Queue.pop

let peek t = if is_empty t then None else Some (Queue.peek t)

let peek_exn = Queue.peek

let clear = Queue.clear

let copy = Queue.copy

let length = Queue.length

let iter t ~f = Queue.iter f t

let fold t ~init ~f = Queue.fold f init t

let transfer ~src ~dst = Queue.transfer src dst

let filter_map t ~f =
  let res = create () in
  iter t ~f:(fun a ->
    match f a with
    | None -> ()
    | Some b -> enqueue res b);
  res;
;;

let map t ~f =
  let res = create () in
  iter t ~f:(fun a -> enqueue res (f a));
  res;
;;

let filter_inplace q ~f =
  let q' = create () in
  transfer ~src:q ~dst:q';
  iter q' ~f:(fun x -> if f x then enqueue q x)

let to_list t = List.rev (fold t ~init:[] ~f:(fun acc elem -> elem::acc))

let of_list list =
  let t = create () in
  List.iter list ~f:(fun x -> enqueue t x);
  t

let of_array array =
  let t = create () in
  Array.iter array ~f:(fun x -> enqueue t x);
  t
;;

let to_array t =
  match length t with
  | 0 -> [||]
  | len ->
    let arr = Array.create len (peek_exn t) in
    let i = ref 0 in
    iter t ~f:(fun v ->
      arr.(!i) <- v;
      incr i);
    arr

let find t ~f =
  with_return (fun r ->
    iter t ~f:(fun x -> if f x then r.return (Some x));
    None)
;;

let exists t ~f = Option.is_some (find t ~f)
let for_all t ~f = not (exists t ~f:(fun x -> not (f x)))

let partial_iter t ~f =
  with_return (fun r ->
    iter t ~f:(fun x ->
      match f x with
      | `Continue -> ()
      | `Stop -> r.return ()))
;;

type 'a sexpable = 'a t


let t_of_sexp a_of_sexp sexp = of_list (list_of_sexp a_of_sexp sexp)
let sexp_of_t sexp_of_a t = sexp_of_list sexp_of_a (to_list t)

type 'a container = 'a t

let container = {
  Container.
  length = length;
  is_empty = is_empty;
  iter = iter;
  fold = fold;
  exists = exists;
  for_all = for_all;
  find = find;
  to_list = to_list;
  to_array = to_array;
}

let fold t ~init ~f = Queue.fold f init t

let to_list t = List.rev (fold t ~init:[] ~f:(fun l x -> x::l))
