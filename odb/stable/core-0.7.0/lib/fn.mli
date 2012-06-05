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

(** various combinators for functions *)

(** A 'pipe' operator. *)
val ( |! ) : 'a -> ( 'a -> 'b) -> 'b

(** produces a function that just returns its first argument *)
val const : 'a -> _ -> 'a

(**
 * [ignore] is the same as [Pervasives.ignore].  It is useful to have here so
 * that code that rebinds [ignore] can still refer to [Fn.ignore].
 *)
external ignore : _ -> unit = "%ignore"

(** Negates a function *)
val non : ('a -> bool) -> 'a -> bool

(** [forever f] runs [f ()] until it throws an exception and returns the
    exception. This function is useful for read_line loops, etc. *)
val forever : (unit -> unit) -> exn

(** The identity function*)
external id : 'a -> 'a = "%identity"

(** function composition *)
val compose : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)

(** reverse the order of arguments for a binary function *)
val flip : ('a -> 'b -> 'c) -> ('b -> 'a -> 'c)
