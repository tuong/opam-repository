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

(* Error-checking mutexes. *)

include Mutex

(** [create] like {!Mutex.create}, but creates an error-checking mutex.
    Locking a mutex twice from the same thread, unlocking an unlocked mutex,
    or unlocking a mutex not held by the thread will result in a [Sys_error]
    exception. *)

external create : unit -> Mutex.t = "unix_create_error_checking_mutex"

let create = create

let phys_equal = Caml.(==)

let equal (t : t) t' = phys_equal t t'

let critical_section l ~f =
  lock l;
  Exn.protect ~f ~finally:(fun () -> unlock l)

let synchronize f =
  let mtx = create () in
  let f' x = critical_section mtx ~f:(fun () -> f x) in
  f'

let update_signal mtx cnd ~f =
  critical_section mtx ~f:(fun () ->
    let res = f () in
    Condition.signal cnd;
    res)

let update_broadcast mtx cnd ~f =
  critical_section mtx ~f:(fun () ->
    let res = f () in
    Condition.broadcast cnd;
    res)

let am_holding_mutex mtx =
  match
    try if try_lock mtx then `Free else `Held_by_other
    with _ -> `Held_by_me
  with
  | `Free -> unlock mtx; false
  | `Held_by_me -> true
  | `Held_by_other -> false
