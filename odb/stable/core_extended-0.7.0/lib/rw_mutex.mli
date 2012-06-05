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

(** Read/write mutexes

    @author Markus Mottl <mmottl\@janestreet.com>
*)

(** {2 Types} *)

(** {3 Real types} *)

(** Type of r/w mutexes *)
type 'pref t

(** {3 Phantom types} *)

(** Preference for readers, writers, or no preference *)
type pref = [ `Readers | `Writers | `NoPref ]

(** Preference kind of read/write mutexes *)
type 'pref kind


(** {2 Phantom values} *)

val r_pref : [ `Readers ] kind
(** [r_pref] preference kind for readers. *)

val w_pref : [ `Writers ] kind
(** [w_pref] preference kind for writers. *)

val np_pref : [ `NoPref ] kind
(** [np_pref] no preference for readers or writers. *)


(** {2 Mutex operations} *)

val create : 'pref kind -> 'pref t
(** [create pref] @return a r/w-mutex with preference kind [pref]. *)

val r_lock : [< pref ] t -> unit
(** [r_lock mtx] locks [mtx] for a reader. *)

val r_unlock : [< pref ] t -> unit
(** [r_unlock mtx] unlocks [mtx] for a reader. *)

val w_lock : [< pref ] t -> unit
(** [w_lock mtx] locks [mtx] for a writer. *)

val w_unlock : [< pref ] t -> unit
(** [w_unlock mtx] unlocks [mtx] for a writer. *)

val try_r_lock : [< pref ] t -> bool
(** [try_r_lock mtx] tries to lock [mtx] for a reader without blocking.
    @return [true] iff [mtx] could be locked, [false] otherwise. *)

val try_w_lock : [< pref ] t -> bool
(** [try_w_lock mtx] tries to lock [mtx] for a writer without blocking.
    @return [true] iff [mtx] could be locked, [false] otherwise. *)

val wrap_r_lock : [< pref ] t -> (unit -> 'a) -> 'a
(** [wrap_r_lock mtx f] locks [mtx] for a reader, executes [f] and
    unlocks the mutex again.  @return the result of [f]. *)

val try_wrap_r_lock : [< pref ] t -> (unit -> 'a) -> 'a option
(** [try_wrap_r_lock mtx f] tries to lock [mtx] for a reader without
    blocking, executes [f] and unlocks the mutex again.  @return [Some
    res], where [res] is the result of [f], iff the mutex could be locked,
    [None] otherwise. *)

val btry_wrap_r_lock : [< pref ] t -> (unit -> unit) -> bool
(** [btry_wrap_r_lock mtx f] tries to lock [mtx] for a reader without
    blocking, executes [f] and unlocks the mutex again.  @return [true]
    iff the mutex could be locked, [false] otherwise. *)

val wrap_w_lock : [< pref ] t -> (unit -> 'a) -> 'a
(** [wrap_w_lock mtx f] locks [mtx] for a writer, executes [f] and
    unlocks the mutex again.  @return the result of [f]. *)

val try_wrap_w_lock : [< pref ] t -> (unit -> 'a) -> 'a option
(** [try_wrap_w_lock mtx f] tries to lock [mtx] for a writer without
    blocking, executes [f] and unlocks the mutex again.  @return [Some
    res], where [res] is the result of [f], iff the mutex could be locked,
    [None] otherwise. *)

val btry_wrap_w_lock : [< pref ] t -> (unit -> unit) -> bool
(** [btry_wrap_w_lock mtx f] tries to lock [mtx] for a writer without
    blocking, executes [f] and unlocks the mutex again.  @return [true]
    iff the mutex could be locked, [false] otherwise. *)
