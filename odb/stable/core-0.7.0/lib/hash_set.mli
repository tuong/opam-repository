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

(* A mutable set of elements *)



type 'a t

val copy : 'a t -> 'a t                 (* preserves the equality function *)
val add : 'a t -> 'a -> unit
val strict_add : 'a t -> 'a -> unit
val remove : 'a t -> 'a -> unit
val strict_remove : 'a t -> 'a -> unit
val clear : 'a t -> unit
val fold : f:('a -> 'b -> 'a) -> init:'a -> 'b t -> 'a
val iter : f:('a -> unit) -> 'a t -> unit
val length : 'a t -> int
val mem : 'a t -> 'a -> bool
val is_empty : 'a t -> bool
val to_list : 'a t -> 'a list
val equal : 'a t -> 'a t -> bool

type 'a hash_set = 'a t

(* A hash set that uses polymorphic comparison *)
module Poly : sig
  type 'a t = 'a hash_set
  include Sexpable.S1 with type 'a sexpable = 'a t
  val create : ?growth_allowed:bool -> ?size:int -> unit -> 'a t
  val of_list : 'a list -> 'a t
end

module Make (H : Core_hashtbl.Key) : sig
  type elem = H.t
  type t = elem hash_set
  val create : ?growth_allowed:bool -> ?size:int -> unit -> t
  val of_list : H.t list -> t
  include Sexpable.S with type sexpable = t
end

module Make_binable (H : sig
  include Core_hashtbl.Key
  include Binable.S with type binable = t
end) : sig
  type elem = H.t
  type t = elem hash_set
  val create : ?growth_allowed:bool -> ?size:int -> unit -> t
  val of_list : H.t list -> t
  include Sexpable.S with type sexpable = t
  include Binable.S with type binable = t
end
