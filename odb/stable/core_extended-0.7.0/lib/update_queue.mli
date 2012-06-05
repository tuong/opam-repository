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


(** A ['state t] keeps track of updates of type ['state -> 'state] queued to it and runs
    them sequentially.

    This has the primary feature that if an update itself schedules another update, that
    other update will be run after the first update has finished.

    For example, consider the code:

    {[let x = create ~init:1 () in
    enqueue_update x (fun x ->
    enqueue_update (fun x -> x + 1);
    x + 1)]}

    At the end, [x]'s state will be 2, as you would expect.  However, suppose you did
    this with an [int ref]:

    {[let x = ref 1 in
    let update_x f = x := f !x in
    update_x (fun x ->
      update_x (fun x -> x + 1);
      x + 1
    )]}

    At the end of this, [!x] would be 1.

    Another feature is that the initial value does not have to be specified at creation.
    If it is not, enqueued updates will be kept around until an initial value is specified
    with [init].
*)

type ('perm, 'state) t

val create : ?init:'state -> unit -> (_, 'state) t
val init : (read_write, 'state) t -> 'state -> unit

val enqueue : (read_write, 'state) t -> ('state -> 'state) -> unit

(** [watch t f] will call [f] every time that that [t]'s state is updated. [f]
    should not call [enqueue_update]. *)
val watch : (_, 'state) t -> f:('state -> unit) -> unit

(* This function will register a watcher with the input [t].  That means the return value
   will not be garbage-collected at least as long as the input [t] is not garbage-collected. *)
val map : (_, 'state1) t -> f:('state1 -> 'state2) -> (read_only,'state2) t

val read_only : (_,'state) t -> (read_only, 'state) t
