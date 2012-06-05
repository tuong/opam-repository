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

(** Floating-point representation and utilities. *)

module type S = sig
  type t
  type outer = t

  include Sexpable.S with type sexpable = t
  include Binable.S with type binable = t
  include Floatable.S with type floatable = t
  include Stringable.S with type stringable = t
  include Hashable.S_binable with type hashable = t
  (* [max] and [min] will return nan if either argument is nan *)
  include Comparable.S_binable with type comparable = t
  (* The results of robust comparisons on [nan] should be considered undefined. *)
  include Robustly_comparable.S with type robustly_comparable = t

  val max_value : t                   (* infinity *)
  val min_value : t                   (* neg_infinity *)
  val zero : t
  val epsilon : t

  val max_finite_value : t
  val min_finite_value : t

  val of_int : int -> t
  
  val to_int : t -> int
  val of_int64 : int64 -> t
  val to_int64 : t -> int64

  (** round_towards_zero_exn raises Invalid_argument when either trying to handle nan or
      trying to handle a float outside the range (-. 2. ** 52., 2. ** 52.)
      (since floats have 52 significant bits) or outside the range
      (float min_int, float max_int) *)
  val round_towards_zero_exn : t -> int           (* closer to 0 *)
  (** round_towards_zero returns None if round_towards_zero_exn raises an exception *)
  val round_towards_zero : t -> int option        (* closer to 0 *)
  val round : t -> t                    (* nearest integer *)
  (* round_down[_exn] rounds towards neg_infinity *)
  val round_down_exn : t -> int
  val round_down : t -> int option
  (* round_up[_exn] rounds toward infinity *)
  val round_up_exn : t -> int
  val round_up : t -> int option
  (** iround_exn raises Invalid_argument in the same cases as round_towards_zero_exn *)
  val iround_exn : t -> int

  (** iround returns None if iround_exn raises an exception *)
  (** [iround t] rounds t to the nearest int.  Returns None when t is too large to round to
      an int. *)
  val iround : t -> int option

  
  (** Ordinary float-only nan test. *)
  val is_nan : t -> bool

  (** Ordinary float-only infinity test. *)
  val is_inf : t -> bool

  (** min that returns the other value if one of the values is a [nan]. *)
  val min_inan : t -> t -> t

  (** max that returns the other value if one of the values is a [nan]. *)
  val max_inan : t -> t -> t

  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  val (/) : t -> t -> t

  (** Returns the fractional part and the whole (i.e. integer) part.  For example, [modf
      (-3.14)] returns [{ fractional = -0.14; integral = -3.; }]! *)
  module Parts : sig
    type t
    val fractional : t -> outer
    val integral : t -> outer
  end
  val modf : t -> Parts.t

  
  (* Caveat: If the absolute value of the input float is very large, then it could be that
     |floor x - ceil x| > 1. *)
  val floor : t -> t              (* rounds down, e.g. [floor (-3.1)] is [-4.] *)
  val ceil : t -> t               (* rounds up, e.g. [floor (-3.1)] is [-3.] *)

  (** [mod_float x y] returns a result with the same sign as [x].  It returns [nan] if [y] is
     [0].  It is basically
     [let mod_float x y = x -. float(truncate(x/.y)) *. y]
     not
     [let mod_float x y = x -. floor(x/.y) *. y]
     and therefore resembles [mod] on integers more than [%].
  *)
  val mod_float : t -> t -> t

  (* mostly for modules that inherit from t, since the infix operators are more convenient *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val neg : t -> t
  val scale : t -> t -> t
  val abs : t -> t

  module Class : sig
    type t =
    | Infinite
    | Nan
    | Normal
    | Subnormal
    | Zero

    include Binable.S with type binable = t
    include Sexpable.S with type sexpable = t
    include Stringable.S with type stringable = t
  end

  (* Don't forget about subnormals: there exist strictly positive numbers representable
     in floating point such that [classify f = Normal && f >. 0.] does *not* hold, and
     likewise for strictly negative numbers.  Here is the number line:

           ...  normals | -ve subnormals | (-/+) zero | +ve subnormals | normals  ...
  *)
  val classify : t -> Class.t

  module Sign : sig
    type t = Neg | Zero | Pos
  end

  val sign : t -> Sign.t

  (* S-expressions contain at most 8 significant digits. *)
  module Terse : sig
    type t = outer
    include Binable.S with type binable = t
    include Sexpable.S with type sexpable = t
    include Stringable.S with type stringable = t
  end
end
