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

(* A thread-safe non-blocking queue of unbounded size.

   The implementation does not use mutexes, so operations are quite fast, just
   a handful of instructions.
*)

type 'a t

(* [create ()] returns an empty queue. *)
val create : unit -> 'a t

(* [create' ()] is a variant of create that returns a pair of functions
   [(dequeue, enqueue)] for operating on the queue. *)
val create' : unit -> (unit -> 'a option) * ('a -> unit)

val dequeue : 'a t -> 'a option
val enqueue : 'a t -> 'a -> unit
