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

open Std_internal

module Mutex = Core_mutex

(** Synchronized queue type *)
type 'a t = {
  ev_q: 'a Queue.t;
  maxsize: int;
  mutex: Mutex.t sexp_opaque;
  not_empty: Condition.t sexp_opaque;
  not_full: Condition.t sexp_opaque;
  finally:unit -> unit;
} with sexp_of

let create maxsize =
  let ev_q = Queue.create () in
  let mutex = Mutex.create () in
  let not_empty = Condition.create () in
  let not_full = Condition.create () in
  {
    ev_q = ev_q;
    mutex = mutex;
    not_empty = not_empty;
    not_full = not_full;
    maxsize = maxsize;
    finally = (fun () ->
      let len = Queue.length ev_q in
      if len <> 0 then Condition.signal not_empty;
      if len < maxsize then Condition.signal not_full;
      Mutex.unlock mutex);
  }
;;

let id () = Thread.id (Thread.self ())


let wrap q run =
  Mutex.lock q.mutex;
  protect ~f:run ~finally:q.finally
;;

let clear q =
  let run () =
    Queue.clear q.ev_q
  in
  wrap q run
;;


let wait_not_full q =
  while Queue.length q.ev_q >= q.maxsize do
    Condition.wait q.not_full q.mutex
  done

let wait_not_empty q =
  while Queue.is_empty q.ev_q do
    Condition.wait q.not_empty q.mutex
  done

(** Pushes an event on the queue if there's room *)
let push q x =
  let run () =
    wait_not_full q;
    Queue.enqueue q.ev_q x;
  in
  wrap q run

(** Pushes an event on the queue, unconditionally, may grow the queue past maxsize *)
let push_uncond q x =
  let run () = Queue.enqueue q.ev_q x in
  wrap q run

(** Pushes an event on the queue if the queue is less than maxsize, otherwise drops it.
  Returns true if the push was successful *)
let push_or_drop q x =
  let run () =
    if Queue.length q.ev_q < q.maxsize then
      begin
        Queue.enqueue q.ev_q x;
        true
      end
    else false
  in
  wrap q run

(** computes the length of the queue *)
let length q =
  let run () = Queue.length q.ev_q in
  wrap q run

(** Pops an event off of the queue, blocking until
    something is available *)
let pop q =
  let run () =
    wait_not_empty q;
    Queue.dequeue_exn q.ev_q
  in
  wrap q run

(** Pops an event off of the queue, blocking until something is available.
    Returns pair of the element found and the length of remaining queue *)
let lpop q =
  let run () =
    wait_not_empty q;
    let el = Queue.dequeue_exn q.ev_q in
    let len = Queue.length q.ev_q in
    el, len
  in
  wrap q run

let transfer_queue_in_uncond q in_q =
  if not (Queue.is_empty in_q) then begin
    let run () =
      Queue.transfer ~src:in_q ~dst:q.ev_q
    in
    wrap q run
  end

let transfer_queue_in q in_q =
  if not (Queue.is_empty in_q) then begin
    let run () =
      wait_not_full q;
      Queue.transfer ~src:in_q ~dst:q.ev_q
    in
    wrap q run
  end


let transfer_queue_nowait_nolock sq q = Queue.transfer ~src:sq.ev_q ~dst:q


let transfer_queue_nowait sq q =
  if not (Queue.is_empty sq.ev_q) then begin
    let run () = transfer_queue_nowait_nolock sq q in
    wrap sq run
  end


let transfer_queue sq q =
  let run () =
    wait_not_empty sq;
    transfer_queue_nowait_nolock sq q
  in
  wrap sq run

(* The external version of wait_not_empty *)
let wait_not_empty sq =
  let run () = wait_not_empty sq in
  wrap sq run
