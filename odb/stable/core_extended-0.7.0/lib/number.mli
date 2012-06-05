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

open Core.Std
open Interfaces

(** Specification of general number properties *)
module type Spec = sig
  type t

  include Comparable with type comparable = t
  include Floatable with type floatable = t
  include Hashable.S_binable with type hashable = t
  include Sexpable with type sexpable = t
  include Stringable with type stringable = t
  include Binable with type binable = t
end

(** Specification of numbers with constraint checks *)
module type Verified_spec = sig
  include Spec

  val check : t -> (unit, string) Result.t
end

(** Signature of numbers that have a verified ([t]) and unverified ([repr])
    representation and a function to verify the latter. *)
module type S = sig
  include Spec

  type repr

  val verify : repr -> t
end

(** Signature of numbers that also have a zero *)
module type S0 = sig
  include S

  val zero : t
end

(** Functor for making constrained numbers from specifications.  This functor
    enforces the invariant through a private type. *)
module Make_verified (Spec : Verified_spec) :
  S
    with type repr = Spec.t
    with type t = private Spec.t

(** Same as {!Make_verified}, but does not enforce invariants.  Useful for
    extending verified numbers with more features before "sealing" the type
    with a private declaration. *)
module Make_verified_unsafe (Spec : Verified_spec) :
  S
    with type repr = Spec.t
    with type t = Spec.t

(** Specification used for standard numbers ([Int.t], [Int32.t], [Int63.t]
    [Int64.t], [Nativeint.t], [Float.t]) to enrich them with [Pos.t],
    [Pos0.t], [Neg.t], and [Neg0.t] modules, and the [Make_bounded] and
    [Make_bounded_unsafe] functors. *)
module type Verified_std_spec = sig
  include Spec

  val module_name : string
  val zero : t
end

(** Signature of standard numbers ([Int.t], [Int32.t], [Int63.t]
    [Int64.t], [Nativeint.t], [Float.t]) to enrich them with [Pos.t],
    [Pos0.t], [Neg.t], and [Neg0.t] modules, and the [Make_bounded] and
    [Make_bounded_unsafe] functors. *)
module type Verified_std = sig
  type repr

  (** Abbreviations *)
  module type S = S with type repr = repr
  module type S0 = S0 with type repr = repr

  (** Positive and negative numbers with and without zero. *)
  module Pos : S with type t = private repr
  module Pos0 : S0 with type t = private repr
  module Neg : S with type t = private repr
  module Neg0 : S0 with type t = private repr

  (** Specification of bounded numbers *)
  module type Bounded_spec = sig
    val name : string
    val lower : repr
    val upper : repr
  end

  (** Signature of bounded numbers *)
  module type Bounded = sig
    include Bounded_spec
    include S
  end

  (** Functor of creating bounded numbers *)
  module Make_bounded (Spec : Bounded_spec) : Bounded with type t = private repr

  (** Unsafe modules and functors that still fully expose the representation
      for extensibility. *)

  module Pos_unsafe : S with type t = repr
  module Pos0_unsafe : S0 with type t = repr
  module Neg_unsafe : S with type t = repr
  module Neg0_unsafe : S0 with type t = repr

  module Make_bounded_unsafe (Spec : Bounded_spec) : Bounded with type t = repr
end

(** Functor for enriching standard numbers ([Int.t], [Int32.t], [Int63.t]
    [Int64.t], [Nativeint.t], [Float.t]) with [Pos.t], [Pos0.t], [Neg.t],
    and [Neg0.t] modules, and the [Make_bounded] and [Make_bounded_unsafe]
    functors. *)
module Make_verified_std (Spec : Verified_std_spec) :
  Verified_std with type repr = Spec.t
