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

(** Semaphores

    @author Markus Mottl <mmottl\@janestreet.com>
*)

(** Type of semaphores *)
type 'a t

val init : 'a option -> 'a t
(** [init v] initializes a semaphore with an optional value [v].  If it is
    [Some x], then {!Semaphore.wait} will return immediately with [x],
    otherwise it will block until {!Semaphore.signal} is called. *)

val signal : 'a t -> 'a -> unit
(** [signal sem v] allows one thread blocked in {!Semaphore.wait} on
    semaphore [sem] to continue.  The semaphore will then block again
    further threads. *)

val wait : 'a t -> 'a
(** [wait sem] blocks the calling thread on semaphore [sem] if it was not
    initialized with [Some x] or not signalled before.  The semaphore
    is reset to [None], i.e. calling [wait] again will block unless the
    semaphore was signalled inbetween. *)

val get : 'a t -> 'a option
(** [get sem] @return [None] if semaphore is not set, [Some value]
    otherwise.  The semaphore is reset to [None], and a subsequent wait
    will block again. *)

val look : 'a t -> 'a option
(** [look sem]
    @return [None] if semaphore is not set, [Some value]
    otherwise.  The state of the semaphore remains unchanged. *)
