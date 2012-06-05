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



open Sexplib



module type Elt = sig
  type t
  include Sexpable.S with type sexpable = t

  val compare : t -> t -> int
end

module type Types = sig
  type +'e elt
  type +'e t
end

module Gen(T : Types) = struct
  open T
  module type S = sig
    val empty: 'e t
    val is_empty: 'e t -> bool
    val mem: 'e t -> 'e elt -> bool
    val add: 'e t -> 'e elt -> 'e t
    val singleton: 'e elt -> 'e t
    val remove: 'e t -> 'e elt -> 'e t
    val union: 'e t -> 'e t -> 'e t
    val union_list : 'e t list -> 'e t
    val inter: 'e t -> 'e t -> 'e t
    val diff: 'e t -> 'e t -> 'e t
    val compare: 'e t -> 'e t -> int
    val equal: 'e t -> 'e t -> bool
    val subset: 'e t -> 'e t -> bool
    
    val iter: f:('e elt -> unit) -> 'e t -> unit
    val fold: f:('e elt -> 'a -> 'a) -> 'e t -> init:'a -> 'a
    val fold_until: f:('e elt -> 'a -> [`Continue of 'a | `Stop of 'a]) -> 'e t -> init:'a -> 'a
    val for_all: f:('e elt -> bool) -> 'e t -> bool
    val exists: f:('e elt -> bool) -> 'e t -> bool
    val filter: f:('e elt -> bool) -> 'e t -> 'e t
    val partition: f:('e elt -> bool) -> 'e t -> 'e t * 'e t
    
    val cardinal: 'e t -> int
    val length : _ t -> int
    val elements: 'e t -> 'e elt list
    val min_elt: 'e t -> 'e elt option
    val min_elt_exn: 'e t -> 'e elt
    val max_elt: 'e t -> 'e elt option
    val max_elt_exn: 'e t -> 'e elt
    val choose: 'e t -> 'e elt option
    val choose_exn: 'e t -> 'e elt
    val of_list: 'e elt list -> 'e t
    val to_list: 'e t -> 'e elt list
    val of_array: 'e elt array -> 'e t
    val to_array: 'e t -> 'e elt array
    val split: 'e elt -> 'e t -> 'e t * bool * 'e t
    val group_by: 'e t -> equiv:('e elt -> 'e elt -> bool) -> 'e t list
    val find: 'e t -> f:('e elt -> bool) -> 'e elt option
    val find_exn: 'e t -> f:('e elt -> bool) -> 'e elt
    (* Returns the ith smallest element in the set in O(log n) time.  The smallest
       element is element 0. *)
    val find_index : 'e t -> int -> 'e elt option
    val remove_index : 'e t -> int -> 'e t
  end
end

module type S = sig
  type elt
  type t
  module T : Types with type 'a elt = elt with type 'e t = t
  include Sexpable.S with type sexpable = t
  val empty: t
  val is_empty: t -> bool
  val mem: t -> elt -> bool
  val add: t -> elt -> t
  val singleton: elt -> t
  val remove: t -> elt -> t
  val union: t -> t -> t
  val union_list : t list -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val subset: t -> t -> bool
  val iter: f:(elt -> unit) -> t -> unit
  val fold: f:(elt -> 'a -> 'a) -> t -> init:'a -> 'a
  val fold_until: f:(elt -> 'a -> [`Continue of 'a | `Stop of 'a]) -> t -> init:'a -> 'a
  val for_all: f:(elt -> bool) -> t -> bool
  val exists: f:(elt -> bool) -> t -> bool
  val filter: f:(elt -> bool) -> t -> t
  val partition: f:(elt -> bool) -> t -> t * t
  val cardinal: t -> int
  val length : t -> int
  val elements: t -> elt list
  val min_elt: t -> elt option
  val min_elt_exn: t -> elt
  val max_elt: t -> elt option
  val max_elt_exn: t -> elt
  val choose: t -> elt option
  val choose_exn: t -> elt
  val of_list: elt list -> t
  val to_list: t -> elt list
  val of_array: elt array -> t
  val to_array: t -> elt array
  val split: elt -> t -> t * bool * t
  val group_by: t -> equiv:(elt -> elt -> bool) -> t list
  val find: t -> f:(elt -> bool) -> elt option
  val find_exn: t -> f:(elt -> bool) -> elt
  val find_index : t -> int -> elt option
  val remove_index : t -> int -> t
end

module type S1 = sig
  type +'elt t
  module T : Types with type 'a elt = 'a with type 'e t = 'e t
  include Sexpable.S1 with type 'elt sexpable = 'elt t
  include Binable.S1 with type 'elt binable = 'elt t
  val empty: 'elt t
  val is_empty: 'elt t -> bool
  val mem: 'elt t -> 'elt -> bool
  val add: 'elt t -> 'elt -> 'elt t
  val singleton: 'elt -> 'elt t
  val remove: 'elt t -> 'elt -> 'elt t
  val union: 'elt t -> 'elt t -> 'elt t
  val union_list : 'elt t list -> 'elt t
  val inter: 'elt t -> 'elt t -> 'elt t
  val diff: 'elt t -> 'elt t -> 'elt t
  val compare: 'elt t -> 'elt t -> int
  val equal: 'elt t -> 'elt t -> bool
  val subset: 'elt t -> 'elt t -> bool
  val iter: f:('elt -> unit) -> 'elt t -> unit
  val fold: f:('elt -> 'a -> 'a) -> 'elt t -> init:'a -> 'a
  val fold_until: f:('elt -> 'a -> [`Continue of 'a | `Stop of 'a]) -> 'elt t -> init:'a -> 'a
  val for_all: f:('elt -> bool) -> 'elt t -> bool
  val exists: f:('elt -> bool) -> 'elt t -> bool
  val filter: f:('elt -> bool) -> 'elt t -> 'elt t
  val filter_map: f:('elt -> 'a option) -> 'elt t -> 'a t
  val partition: f:('elt -> bool) -> 'elt t -> 'elt t * 'elt t
  val cardinal: _ t -> int
  val length : _ t -> int
  val elements: 'elt t -> 'elt list
  val min_elt: 'elt t -> 'elt option
  val min_elt_exn: 'elt t -> 'elt
  val max_elt: 'elt t -> 'elt option
  val max_elt_exn: 'elt t -> 'elt
  val choose: 'elt t -> 'elt option
  val choose_exn: 'elt t -> 'elt
  val of_list: 'elt list -> 'elt t
  val to_list: 'elt t -> 'elt list
  val of_array: 'elt array -> 'elt t
  val to_array: 'elt t -> 'elt array
  val map: f:('a -> 'b) -> 'a t -> 'b t
  val split: 'elt -> 'elt t -> 'elt t * bool * 'elt t
  val group_by: 'elt t -> equiv:('elt -> 'elt -> bool) -> 'elt t list
  val find: 'elt t -> f:('elt -> bool) -> 'elt option
  val find_exn: 'elt t -> f:('elt -> bool) -> 'elt
  val find_index : 'elt t -> int -> 'elt option
  val remove_index : 'elt t -> int -> 'elt t
end

module type Gen = sig
  module T : Types
  include Gen(T).S
end

module Check_S (M : S)   = (M : Gen(M.T).S)
module Check_S1 (M : S1) = (M : Gen(M.T).S)
