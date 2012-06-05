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



(* These are just the creation functions for non-polymorphic Hash_sets.  Most of the
   functions live directly in Hash_set.  E.g.

   let my_set = Int.Hash_set.create in
   Hash_set.add my_set 3
*)

module type S = sig
  type elem
  type t = elem Hash_set.t
  include Sexpable.S with type sexpable = t
  val create : ?growth_allowed:bool -> ?size:int -> unit -> t
  val of_list : elem list -> t
end

module type S_binable = sig
  type elem
  type t = elem Hash_set.t
  include Sexpable.S with type sexpable = t
  include Binable.S with type binable = t
  val create : ?growth_allowed:bool -> ?size:int -> unit -> t
  val of_list : elem list -> t
end
