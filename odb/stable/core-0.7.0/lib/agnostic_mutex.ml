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

(* NOTE: this module must use the OCaml standard library for mutexes
   and condition variables, not error checking mutexes!  Otherwise e.g. a
   locked mutex after a fork cannot be unlocked anymore if its locking
   thread has died. *)

module Mutex = Caml.Mutex

(* The state of the agnostic mutex (locked / unlocked) is represented by
   t.locked.  This state is protected by an ordinary mutex t.mtx, which
   is only ever held for a few instructions.  If thread coordination
   is required, e.g. when we try to lock an already locked agnostic
   mutex, Condition.wait and Condition.signal are used with t.cnd.
   Condition.wait atomically releases an ordinary mutex and waits for a
   signal to wake up.  Condition.signal is used to cause such a wakeup.
   Condition.wait then reacquires the mutex.  The condition always needs
   to be checked in a loop until it is satisfied. *)

type t =
  { mtx : Mutex.t;
    cnd : Condition.t;
    mutable locked : bool;
  }

let create () =
  { mtx = Mutex.create ();
    cnd = Condition.create ();
    locked = false;
  }

let phys_equal = Caml.(==)

let equal (t : t) t' = phys_equal t t'

let wrap_mutex mtx ~f =
  Mutex.lock mtx;
  Exn.protect ~f ~finally:(fun () -> Mutex.unlock mtx)

let try_lock t =
  wrap_mutex t.mtx ~f:(fun () ->
    let was_locked = t.locked in
    t.locked <- true;
    not was_locked)

let lock t =
  wrap_mutex t.mtx ~f:(fun () ->
    while t.locked do Condition.wait t.cnd t.mtx done;
    t.locked <- true)

let unlock t =
  wrap_mutex t.mtx ~f:(fun () ->
    if t.locked then begin
      t.locked <- false;
      Condition.signal t.cnd;
    end
    else failwith "Agnostic_mutex.unlock: already unlocked")

let critical_section t ~f =
  lock t;
  Exn.protect ~f ~finally:(fun () -> unlock t)
