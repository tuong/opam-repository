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

open Std_internal

(** Module for simple closed intervals over arbitrary types that are ordered
    correctly using polymorphic compare. *)

module type S = Interval_intf.S
module type S1 = Interval_intf.S1

include Interval_intf.S1

module Make(M : sig
  type t
  include Comparable.S with type comparable = t
  (* Sexps are () for empty interval and (3 5) for an interval containing 3, 4, and 5. *)
  include Sexpable.S with type sexpable = t
  include Binable.S with type binable = t
end)
  : S with type bound = M.t and type 'a poly_t = M.t t


module Float : S with type bound = Float.t    and type 'a poly_t = Float.t    t
module Int   : S with type bound = Core_int.t and type 'a poly_t = Core_int.t t
module Time : sig
  include      S with type bound = Time.t     and type 'a poly_t = Time.t     t

  (* [create_ending_after (od1, od2) ~now] returns the smallest interval [(t1 t2)]
     with minimum [t2] such that [t2 >= ubound], [to_ofday t1 = od1], and
     [to_ofday t2 = od2].  Contrary to the name, it is not guaranteed that
     [contains (t1 t2) now], which will be false iff there is no interval
     containing [now] with [to_ofday t1 = od1] and [to_ofday t2 = od1] . *)
  val create_ending_after : (Ofday.t * Ofday.t) -> now:   Time.t -> t

  (* [create_ending_before (od1, od2) ~ubound] returns the smallest interval [(t1 t2)]
     with maximum [t2] such that [t2 <= ubound], [to_ofday t1 = od1], and
     [to_ofday t2 = od2]. *)
  val create_ending_before     : (Ofday.t * Ofday.t) -> ubound:Time.t -> t
end
