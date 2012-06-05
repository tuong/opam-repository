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

(** The raison d'etre for Space_safe_tuple<N> is that OCaml doesn't properly
    free variables matched in tuple patterns.  If one writes

      let (a, b) = ... in
      ... a ... b ...

    when a and b appear at most once in the subsequent expression
    then this effectively becomes

      let t = ... in
      ... (fst t) ... (snd t) ...

    Hence, references to [a] and [b] keep alive the entire tuple.  This can
    lead to surprising space leaks.

    One notable instance of this bad behaviour is when one writes:

    let _,a = ... in ...a...

    The first element of the tuple, albeit not even being named, is live in the
    subsequent expression.

    By using Space_safe_tuple<N>, one makes it clear to the user where the tuple
    selection happens, and (hopefully) causes them to think about space safety,
    and to write the following if they want to free the tuple.

      let t = ... in
      let a = Space_safe_tuple2.get1 t in
      let b = Space_safe_tuple2.get2 t in
      ... a ... b ...
*)
module T2 : sig
  type ('a, 'b) t
  val create : 'a -> 'b -> ('a, 'b) t
  external get1 : ('a, _) t -> 'a = "%field0"
  external get2 : (_, 'a) t -> 'a = "%field1"
end

module T3 : sig
  type ('a, 'b, 'c) t
  val create : 'a -> 'b -> 'c -> ('a, 'b, 'c) t
  external get1 : ('a, _, _) t -> 'a = "%field0"
  external get2 : (_, 'a, _) t -> 'a = "%field1"
  val get3 : (_, _, 'a) t -> 'a
end
