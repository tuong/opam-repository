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

(** Simple implementation of a polymorphic functional queue *)

(** Performance guarantees:

      - push, top, and bottom are O(1).
      - pop is O(1) amortized.
      - to_list is O(n).
      - length is O(1).
*)

(** Invariants:

      - iff queue is not empty, outlist is not empty
      - iff queue has more than 1 element, then inlist is not empty
      - queue.length = List.length queue.outlist + List.length queue.inlist
*)

open Std_internal

exception Empty with sexp

type 'a t = { inlist : 'a list; outlist : 'a list; length : int } with bin_io, sexp

let test_invariants queue =
  let n_out = List.length queue.outlist in
  let n_in = List.length queue.inlist in
  assert (queue.length = n_out + n_in);
  assert (queue.length = 0 || n_out <> 0);
  assert (queue.length <= 1 || n_in <> 0)

let empty = { inlist = []; outlist = []; length = 0 }

let push el queue =
  let inlist, outlist =
    if queue.length = 0 then [], [el]
    else el :: queue.inlist, queue.outlist
  in
  { inlist = inlist; outlist = outlist; length = queue.length + 1 }

(** pushes el on the top of the queue, effectively making it
    the least recently enqueued element *)
let push_top el queue =
  let inlist, outlist =
    if queue.inlist = [] then List.rev queue.outlist, [el]
    else queue.inlist, el :: queue.outlist
  in
  { inlist = inlist; outlist = outlist; length = queue.length + 1 }

(** same as push *)
let enq = push

(** returns bottom (most-recently enqueued) item  *)
let bot_exn queue =
  match queue.inlist, queue.outlist with
  | [], [x] | x :: _, _ -> x
  | [], [] -> raise Empty
  | [], _ :: _ :: _ ->
      raise (Bug "Fqueue.bot_exn: empty inlist and outlist with len > 1")

let bot queue = try Some (bot_exn queue) with Empty -> None

(** returns top (least-recently enqueued) item  *)
let top_exn queue =
  match queue.outlist with
  | x :: _ -> x
  | [] -> raise Empty

let top queue = try Some (top_exn queue) with Empty -> None

(** returns top of queue and queue with top removed  *)
let pop_exn queue =
  let x, inlist, outlist =
    match queue.inlist, queue.outlist with
    | [_] as inlist, [x] -> x, [], inlist
    | y :: ytl, [x] -> x, [y], List.rev ytl
    | inlist, x :: xtl -> x, inlist, xtl
    | [], [] -> raise Empty
    | _ :: _, [] -> raise (Bug "Fqueue.pop_exn: outlist empty, inlist not")
  in
  x, { inlist = inlist; outlist = outlist; length = queue.length - 1 }

let pop queue = try Some (pop_exn queue) with Empty -> None

(** same as pop *)
let deq = pop

let deq_exn = pop_exn

(** returns queue with top removed *)
let discard_exn queue = snd (pop_exn queue)

let to_list queue = List.append queue.outlist (List.rev queue.inlist)

let sexp_of_t sexp_of_a q = Sexplib.Conv.sexp_of_list sexp_of_a (to_list q)

let length queue = queue.length

let is_empty queue = queue.length = 0
