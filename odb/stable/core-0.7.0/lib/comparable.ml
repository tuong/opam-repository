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


module type Infix = sig
  type comparable
  val ( >= ) : comparable -> comparable -> bool
  val ( <= ) : comparable -> comparable -> bool
  val ( = ) : comparable -> comparable -> bool
  val ( > ) : comparable -> comparable -> bool
  val ( < ) : comparable -> comparable -> bool
  val ( <> ) : comparable -> comparable -> bool
end

module type S_common = sig
  include Infix
  
  val equal : comparable -> comparable -> bool
  val compare : comparable -> comparable -> int
  val ascending : comparable -> comparable -> int
  val descending : comparable -> comparable -> int
  val min : comparable -> comparable -> comparable
  val max : comparable -> comparable -> comparable
end

module type S = sig
  include S_common

  module Map : Core_map.S with type key = comparable
  module Set : Core_set.S with type elt = comparable
end

module type S_binable = sig
  include S_common

  module Map : Core_map.S_binable with type key = comparable
  module Set : Core_set.S_binable with type elt = comparable
end

module Poly (T : sig
  type t
  include Sexpable.S with type sexpable = t
end) : S with type comparable = T.t = struct
  type comparable = T.t
  include Pervasives                    (* for Infix *)
  let ascending = compare
  let descending x y = compare y x
  let equal = (=)
  module C = struct
    include T
    let compare = compare
  end
  module Map = Core_map.Make (C)
  module Set = Core_set.Make (C)
end

module Make_common (T : sig
  type t
  include Sexpable.S with type sexpable = t
  val compare : t -> t -> int
end) = struct
  type comparable = T.t

  let compare = T.compare
  let ascending = compare
  let descending t t' = compare t' t

  module Infix = struct
    let (>) a b = compare a b > 0
    let (<) a b = compare a b < 0
    let (>=) a b = compare a b >= 0
    let (<=) a b = compare a b <= 0
    let (=) a b = compare a b = 0
    let (<>) a b = compare a b <> 0
  end
  include Infix

  let equal = (=)
  let min t t' = if t <= t' then t else t'
  let max t t' = if t >= t' then t else t'
end

module Make (T : sig
  type t
  include Sexpable.S with type sexpable = t
  val compare : t -> t -> int
end) : S with type comparable = T.t = struct
  include Make_common (T)

  module Map = Core_map.Make (T)
  module Set = Core_set.Make (T)
end

module Make_binable (T : sig
  type t
  include Sexpable.S with type sexpable = t
  include Binable.S with type binable = t
  val compare : t -> t -> int
end) : S_binable with type comparable = T.t = struct
  include Make_common (T)

  module Map = Core_map.Make_binable (T)
  module Set = Core_set.Make_binable (T)
end

(** Inherit comparability from a component. *)
module Inherit (C : S)
  (T : sig
    type t
    include Sexpable.S with type sexpable = t
    val component : t -> C.comparable
  end)
  : S with type comparable = T.t = struct

    type comparable = T.t
    (* We write [binary] in this way for performance reasons.  It is always
     * applied to one argument and builds a two-argument closure.
     *)
    let binary f = (); fun t t' -> f (T.component t) (T.component t')
    let compare = binary C.compare
    let (>=) = binary C.(>=)
    let (<=) = binary C.(<=)
    let (=) = binary C.(=)
    let equal = (=)
    let (>) = binary C.(>)
    let (<) = binary C.(<)
    let (<>) = binary C.(<>)
    let ascending = binary C.ascending
    let descending = binary C.descending
    let min t t' = if t <= t' then t else t'
    let max t t' = if t >= t' then t else t'

    module M = struct
      include T
      let compare = compare
    end

    module Map = Core_map.Make (M)
    module Set = Core_set.Make (M)
end

(* compare [x] and [y] lexicographically using functions in the list [cmps] *)
let lexicographic cmps x y =
  let rec loop = function
    | cmp :: cmps -> let res = cmp x y in if res = 0 then loop cmps else res
    | [] -> 0
  in
  loop cmps
;;
