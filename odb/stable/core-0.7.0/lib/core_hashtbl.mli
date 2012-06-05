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

(* Core_hashtbl is a reimplementation of the standard MoreLabels.Hashtbl.  Its worst case
   time complexity is O(log(N)) for lookups and additions, unlike the standard
   MoreLabels.Hashtbl, which is O(N)

   A hash table is implemented as an array of AVL trees (see [Avltree]). If
   [growth_allowed] (default true) is false then [size] is the final size of
   the array, the table can always hold more elements than [size], however they
   will all go into tree nodes. If it is true (default) then the array will
   double in size when the number of elements in the table reaches twice the
   size of the array. When this happens all existing elements will be
   reinserted, which can take a long time. If you care about latency set [size]
   and [growth_allowed=false] if possible.

   We have three kinds of hash table modules:

     Hashtbl
     Hashtbl.Poly
     Key.Table       (a class of similar modules)

   There are three kinds of hash-table functions:

     creation from nothing (create, of_alist)
     sexp converters (t_of_sexp, sexp_of_t, and bin_io too)
     accessors and mappers (fold, mem, find, map, filter_map, ...)

   Here is a table showing what classes of functions are available in each kind
   of hash-table module:

                    creation   sexp-conv   accessors
     Hashtbl                       X'          X
     Hashtbl.Poly      X           X
     Key.Table         X           X           X'

   Those entries marked with X' are there only for historical reasons, and at
   will be eliminated at some point.  The upshot is that one should use
   [Hashtbl] for accessors, [Hashtbl.Poly] for hash-table creation and sexp
   conversion using polymorphic compare/hash, and [Key.Table] for hash-table
   creation and sexp conversion using [Key.compare] and [Key.hash].
*)

(** For many students of ocaml, using hashtables is complicated by the
    functors.  Here are a few tips: *)

(** For a list of hashtable functions see [Hashtbl_intf.S].*)



(** To create a hashtable with string keys use String.Table.
   {[
    let table = String.Table.create () ~size:4 in
    List.iter ~f:(fun (key, data) -> String.Table.replace table ~key ~data)
      [ ("A", 1); ("B", 2); ("C", 3); ];
    String.Table.find table "C" ]}
    Here 4 need only be a guess at the hashtable's future size.
    There are other similar pre-made hashtables, eg
      Int63.Table or Symbol.Reuters.Table. *)

(** To create a hashtable with a custom key type use Hashable.
    {[
    module Key = struct
      module T = struct
        type t = String.t * Int63.t with sexp
        type sexpable = t
        let equal = (=)
        let hash = Hashtbl.hash
      end
      include T
      include Hashable.Make (T)
    end
    let table = Key.Table.create () ~size:4 in
    List.iter ~f:(fun (key, data) -> Key.Table.replace table ~key ~data)
      [ (("pi", Int63.zero), 3.41459);
        (("e", Int63.minus_one), 2.71828);
        (("Euler", Int63.one), 0.577215);
      ];
    Key.Table.find table ("pi", Int63.zero)]}
    Performance {i may} improve if you define [equal] and [hash] explicitly, eg:
    {[
    let equal (x, y) (x', y') = String.(=) x x' && Int63.(=) y y'
    let hash (x, y) = String.hash x + Int63.hash y * 65599 ]} *)

open Core_hashtbl_intf

module type Key = Key

val hash : 'a -> int
val hash_param : int -> int -> 'a -> int

module T : sig
  type ('a, 'b) t
end

type ('a, 'b) t = ('a, 'b) T.t

include Access_sig (T) (Key_poly).S



include Binable.S2 with type ('a, 'b) binable = ('a, 'b) t
include Sexpable.S2 with type ('a, 'b) sexpable = ('a, 'b) t

module Poly : sig
  include Binable.S2 with type ('a, 'b) binable = ('a, 'b) t
  include Sexpable.S2 with type ('a, 'b) sexpable = ('a, 'b) t
  include Create_sig (T) (Key_poly).S
end

module Make (Key : Key) : Monomorphic (T) (Key).S

module Make_binable (Key : sig
  include Key
  include Binable.S with type binable = t
end) : sig
  include Monomorphic (T) (Key).S

  include Binable.S1 with type 'v binable = 'v t
end
