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

(** Agnostic mutexes do not remember the thread by which they got locked.
    Therefore anybody can unlock them, even if they have not locked them.
    This also means that they will work in child processes: they are
    fork-safe.  The only error-check is that unlocked agnostic mutexes
    cannot be unlocked again, this will raise a [Failure]-exception.

    Though standard OCaml-mutexes (they do not check for errors) could
    in principle be used, too, on Linux systems, because their semantics
    happens to be almost equivalent, this is not recommended.
*)

type t

val create : unit -> t
val equal : t -> t -> bool
val lock : t -> unit
val try_lock : t -> bool
val unlock : t -> unit
val critical_section : t -> f:(unit -> 'a) -> 'a
