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

type t = Condition.t

val create : unit -> t
val equal : t -> t -> bool
val wait : t -> Mutex.t -> unit

(** [timedwait cond mtx timeout] waits on condition variable [cond]
    with mutex [mtx] until either the condition is signalled, or until
    [timeout] expires.  Note that [timeout] is an absolute Unix-time to
    prevent time-related race conditions.

    @return [false] iff the timer expired, but this does not mean that
    the condition is not true due to an unavoidable race condition in
    the system call.

    See [man pthread_cond_timedwait] for details.
*)
val timedwait : t -> Mutex.t -> Time.t -> bool

val signal : t -> unit
val broadcast : t -> unit
