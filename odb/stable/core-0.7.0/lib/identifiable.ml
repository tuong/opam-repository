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

(** This module type is used to create various opaque identifier types. *)
module type S = sig
  type identifiable
  include Stringable.S with type stringable = identifiable
  include Comparable.S with type comparable = identifiable
  include Hashable.S_binable with type hashable = identifiable
  include Sexpable.S with type sexpable = identifiable
  include Binable.S with type binable = identifiable
  val pp : Format.formatter -> identifiable -> unit  (* pretty print for top-level *)
end

(** [Of_stringable] creates an identiable that uses string conversions for
    binable, sexpable, equality, hash, compare, and pp.
    Should only be used for modules where to_string is a cheap operation
    or where performance is not critical. *)
module Of_stringable (T : sig
  type t
  include Stringable.S with type stringable = t
end) = struct
  module T' = struct
    include T
    include Binable.Of_stringable (T)
    include Sexpable.Of_stringable (T)
    let equal t t' = Core_string.equal (T.to_string t) (T.to_string t')
    let hash t = Core_string.hash (T.to_string t)
    let compare t t' = Core_string.compare (T.to_string t) (T.to_string t')
  end
  include Binable.Of_stringable (T)
  include Comparable.Make (T')
  include Hashable.Make_binable (T')
  include Sexpable.Of_stringable (T)
  type identifiable = T.t
  let pp formatter t = Core_string.pp formatter (T.to_string t)
end
