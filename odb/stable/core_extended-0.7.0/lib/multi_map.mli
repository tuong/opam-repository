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

(**
   Maps with mutliple bindings.

   This is a map that allows multiple binding. Each key can have several values
   associated to it.
*)
open Core.Std

type ('k,'v) t
include Sexpable.S2 with type ('a,'b) sexpable = ('a,'b) t

val empty     : ('k,'v) t
val singleton : 'k -> 'v -> ('k,'v) t
val is_empty  : ('k,'v) t -> bool
val add       : key:'k -> data:'v -> ('k, 'v) t -> ('k, 'v) t

val find      : ('k, 'v) t -> 'k -> 'v list
  (** [find m key] returns all the elements that where added to [key] in [m] in
      the reverse order in which they where added. If no element where added an
      empty list is returned.*)

val remove    : ('k, 'v) t -> 'k -> ('k, 'v) t
  (** [remove m key] Remove all the values associated the key [key] in [m]*)

val set       : key:'k -> data:'v list -> ('k, 'v) t -> ('k, 'v) t

val mem       : ('k, 'v) t -> 'k -> bool
  (** [mem m key] returns true if [key] has at last one value associated to it
      in [m] *)

val keys      : ('k, 'v) t -> 'k list
  (** Returns all the non-empty keys in [m]. *)

val iter      :
  f:(key:'k -> data:'v -> unit)
  -> ('k, 'v) t
  -> unit

val map: f:('a -> 'b) -> ('k,'a) t -> ('k,'b) t

val mapi: f:(key:'k -> data:'a -> 'b) -> ('k,'a) t -> ('k,'b) t

val fold      :
  f:(key:'k -> data:'v -> 'a -> 'a)
  -> ('k, 'v) t
  -> init:'a
  -> 'a

val filter : f:(key:'k -> data:'v -> bool) -> ('k,'v) t -> ('k,'v) t

val reduce : f:('v list -> 'r) -> ('k,'v) t -> ('k,'r) Map.t

(*
val data      : ('a, 'b) t -> 'b list
val to_alist  : ('a, 'b) t -> ('a * 'b list) list
val of_list   : ('a * 'b) list -> ('a, 'b) t
val for_all   : f:('a list -> bool) -> ('b, 'a) t -> bool
val exists    : f:('a list -> bool) -> ('b, 'a) t -> bool
val to_map    : ('a, 'b) t -> ('a, 'b list) Map.t
val of_map    : ('a, 'b list) Map.t -> ('a, 'b) t
  *)
