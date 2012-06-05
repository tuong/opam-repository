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

open Bin_prot.Std
module List = Core_list

exception Empty with sexp

type 'a t = {
  mutable elts : 'a list;
  mutable length : int;
} with bin_io

type 'a binable = 'a t
type 'a container = 'a t
type 'a sexpable = 'a t

let invariant t =
  assert (t.length = List.length t.elts);
;;

let create () = { elts = []; length = 0; }

(* We always want to set elts and length at the same time.  Having a function
 * to do so helps us to remember.
 *)
let set t elts length = t.elts <- elts; t.length <- length

let push t x = set t (x :: t.elts) (t.length + 1)

let pop_exn t =
  match t.elts with
  | [] -> raise Empty
  | x :: l -> set t l (t.length - 1); x
;;

let pop t = try Some (pop_exn t) with Empty -> None

let top_exn t =
  match t.elts with
  | [] -> raise Empty
  | x :: _ -> x
;;

let top t = try Some (top_exn t) with Empty -> None


let clear t = set t [] 0

let copy t = { elts = t.elts; length = t.length; }

let length t = t.length

let is_empty t = t.length = 0

let iter t ~f = List.iter t.elts ~f

let fold t ~init ~f = List.fold t.elts ~init ~f

let exists t ~f = List.exists t.elts ~f

let for_all t ~f = List.for_all t.elts ~f

let find t ~f = List.find t.elts ~f

let to_list t = t.elts

let of_list l = { elts = l; length = List.length l }

let to_array t = Array.of_list t.elts

let sexp_of_t sexp_of_a t = Sexplib.Conv.sexp_of_list sexp_of_a (to_list t)

let t_of_sexp a_of_sexp sexp =
  let elts = Sexplib.Conv.list_of_sexp a_of_sexp sexp in
  { elts = elts; length = List.length elts; }
;;

let until_empty t f =
  let rec loop () = if t.length > 0 then (f (pop_exn t); loop ()) in
  loop ()
;;

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
