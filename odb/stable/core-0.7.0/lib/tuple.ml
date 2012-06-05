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

module T2 = struct
  type ('a, 'b) t = 'a * 'b with sexp

  type ('a, 'b) sexpable = ('a, 'b) t

  let create a b = (a, b)

  let curry f = (); fun a b -> f (a, b)

  let uncurry f = (); fun (a,b) -> f a b

  external get1 : ('a, _) t -> 'a = "%field0"
  external get2 : (_, 'a) t -> 'a = "%field1"

  let map1 ~f (x,y) = (f x, y)

  let map2 ~f (x,y) = (x, f y)

  let compare ~cmp1 ~cmp2 =
    fun (x, y) (x', y') ->
      match cmp1 x x' with
      | 0 -> cmp2 y y'
      | i -> i
  ;;

  let swap (a, b) = (b, a)
end

module T3 = struct
  type ('a, 'b, 'c) t = 'a * 'b * 'c with sexp

  type ('a, 'b, 'c) sexpable = ('a, 'b, 'c) t

  let create a b c = (a, b, c)

  let curry f = (); fun a b c -> f (a,b,c)

  let uncurry f = (); fun (a,b,c) -> f a b c

  let map1 ~f (x,y,z) = (f x, y, z)

  let map2 ~f (x,y,z) = (x, f y, z)

  let map3 ~f (x,y,z) = (x, y, f z)

  external get1 : ('a, _, _) t -> 'a = "%field0"
  external get2 : (_, 'a, _) t -> 'a = "%field1"
  (* There's no %field2....*)
  let get3 (_, _, a) = a

  (* lexicographic comparison  *)
  let compare ~cmp1 ~cmp2 ~cmp3 =
    fun (x, y, z) (x', y', z') ->
      let c1 = cmp1 x x' in
      if c1 <> 0 then c1 else
        let c2 = cmp2 y y' in
        if c2 <> 0 then c2 else
          cmp3 z z'
  ;;

end

module type Comparable_sexpable = sig
  include Comparable.S
  include Sexpable.S with type sexpable = comparable
end

module type Hashable_sexpable = sig
  include Hashable.S
  val compare : hashable -> hashable -> int
  include Sexpable.S with type sexpable = hashable
end

module Sexpable (S1 : Sexpable.S) (S2 : Sexpable.S) = struct
  module S1 = struct include S1 type t = S1.sexpable end
  module S2 = struct include S2 type t = S2.sexpable end
  module T = struct type t = S1.t * S2.t with sexp end

  type sexpable = T.t
  let sexp_of_t = T.sexp_of_t
  let t_of_sexp = T.t_of_sexp
end

module Comparable (S1 : Comparable_sexpable) (S2 : Comparable_sexpable) = struct
  module T = struct
    type t = S1.comparable * S2.comparable

    let compare (s1, s2) (s1', s2') =
      match S1.compare s1 s1' with
      | 0 -> S2.compare s2 s2'
      | x -> x

    include Sexpable (S1) (S2)
  end

  include Comparable.Make (T)
end

module Hashable (S1 : Hashable_sexpable) (S2 : Hashable_sexpable) = struct
  module T = struct
    type t = S1.hashable * S2.hashable

    let compare (s1, s2) (s1', s2') =
      match S1.compare s1 s1' with
      | 0 -> S2.compare s2 s2'
      | x -> x

    (* The [land] ensures the return value is the same in 32 and 64-bit processes *)
    let hash (s1, s2) =
      (S1.hash s1 + S2.hash s2 * 65599) land 0x3FFFFFFF

    include Sexpable (S1) (S2)
  end

  include Hashable.Make (T)
end
