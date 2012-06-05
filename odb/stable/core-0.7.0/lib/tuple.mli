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

module T2 : sig
  type ('a, 'b) t = 'a * 'b

  include Sexpable.S2 with type ('a, 'b) sexpable = ('a, 'b) t

  val create : 'a -> 'b -> ('a, 'b) t
  val curry :  (('a, 'b) t -> 'c) -> 'a -> 'b -> 'c
  val uncurry : ('a -> 'b -> 'c) -> ('a, 'b) t -> 'c
  val compare : cmp1: ('a -> 'a -> int) -> cmp2:('b -> 'b -> int)
    -> ('a, 'b) t
    -> ('a, 'b) t
    -> int

  external get1 : ('a, _) t -> 'a = "%field0"
  external get2 : (_, 'a) t -> 'a = "%field1"
  val map1 : f:('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
  val map2 : f:('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

  val swap : ('a, 'b) t -> ('b, 'a) t
end

module T3 : sig
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  include Sexpable.S3 with type ('a, 'b, 'c) sexpable = ('a, 'b, 'c) t

  val create : 'a -> 'b -> 'c -> ('a, 'b, 'c) t
  val curry :  (('a, 'b, 'c) t -> 'd) -> 'a -> 'b -> 'c -> 'd
  val uncurry : ('a -> 'b -> 'c -> 'd) -> ('a, 'b, 'c) t -> 'd
  val compare :
    cmp1:('a -> 'a -> int)
    -> cmp2:('b -> 'b -> int)
    -> cmp3:('c -> 'c -> int)
    -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> int
  external get1 : ('a, _, _) t -> 'a = "%field0"
  external get2 : (_, 'a, _) t -> 'a = "%field1"
  val get3 : (_, _, 'a) t -> 'a
  val map1 : f:('a -> 'd) -> ('a, 'b, 'c) t -> ('d, 'b, 'c) t
  val map2 : f:('b -> 'd) -> ('a, 'b, 'c) t -> ('a, 'd, 'c) t
  val map3 : f:('c -> 'd) -> ('a, 'b, 'c) t -> ('a, 'b, 'd) t
end

(* These functors allow users to write:
   module Foo = struct
     type t = String.t * Int.t
     include Tuple.T2.Comparable (String.t) (Int)
     include Tuple.T2.Hashable (String.t) (Int)
   end
*)

module type Comparable_sexpable = sig
  include Comparable.S
  include Sexpable.S with type sexpable = comparable
end
module Comparable (S1 : Comparable_sexpable) (S2 : Comparable_sexpable)
  : Comparable.S with type comparable = S1.comparable * S2.comparable

module type Hashable_sexpable = sig
  include Hashable.S
  val compare : hashable -> hashable -> int
  include Sexpable.S with type sexpable = hashable
end

module Hashable (S1 : Hashable_sexpable) (S2 : Hashable_sexpable)
  : Hashable.S with type hashable = S1.hashable * S2.hashable

module Sexpable (S1 : Sexpable.S) (S2 : Sexpable.S)
  : Sexpable.S with type sexpable = S1.sexpable * S2.sexpable
