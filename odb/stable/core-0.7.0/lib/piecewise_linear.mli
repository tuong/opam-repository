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

(** piece-wise linear interpolation from float-like types to float *)

(* todo: add to_float/of_float for y values for more generality? *)

module type Key = sig
  type t
  include Floatable with type floatable = t
  include Sexpable with type sexpable = t
  include Binable with type binable = t
end

module type S = sig
  type key

  type t
  include Sexpable with type sexpable = t
  include Binable with type binable = t

  val create : (key * float) list -> (t, string) Result.t
  val get : t -> key -> float
end

module Make (Key : Key) : S with type key = Key.t

module Time : S with type key = Time.t
module Float : S with type key = float
module Int : S with type key = int
