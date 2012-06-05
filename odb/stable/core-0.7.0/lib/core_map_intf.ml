(******************************************************************************
 *                             Core                                           *
 *                                                                            *
 * Copyright (C) 2008- Jane Street Holding, LLC                               *
 *    Contact: opensource@janestreet.com                                      *
 *    WWW: http://www.janestreet.com/ocaml                                    *
 *                                                                            *
 *                                                                            *
 * This file is derived from source code of the Ocaml compiler.               *
 * which has additional copyrights:                                           *
 *                                                                            *
 *    Xavier Leroy, projet Cristal, INRIA Rocquencourt                        *
 *                                                                            *
 *    Copyright 1999 Institut National de Recherche en Informatique et        *
 *    en Automatique.                                                         *
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



open Sexplib

module type Key = sig
  type t
  include Sexpable.S with type sexpable = t

  val compare : t -> t -> int
end

module type Types = sig
  type 'k key
  type ('k, +'v) t
end

(* Gen is a signature functor that generalizes the monomorpic maps and
   polymorphic maps. *)
module Gen (T : Types) = struct
  open T

  

  module type S = sig
    (** the empty map *)
    val empty: ('k, 'v) t

    (** map with one key, data pair *)
    
    val singleton: 'k key -> 'v -> ('k, 'v) t

    (** Test whether a map is empty or not. *)
    val is_empty: ('k, 'v) t -> bool

    
    (** [cardinal map] @return number of elements in [map]. *)
    val cardinal : ('k, 'v) t -> int

    (** returns a new map with the specified new binding;
        if the key was already bound, its previous binding disappears. *)
    
    val add: key:'k key -> data:'v -> ('k, 'v) t -> ('k, 'v) t

    (** if key is not present then add a singleton list, otherwise, cons data
        on the head of the existing list. *)
    val add_multi: key:'k key -> data:'v -> ('k, 'v list) t -> ('k, 'v list) t

    (** [change map key f] updates the given map by changing the value stored
        under [key] according to [f].  Thus, for example, one might write:

        {[change m k (function None -> Some 0 | Some x -> Some (x + 1))]}

        to produce a new map where the integer stored under key [k] is
        incremented by one (treating an unknown key as zero) *)
    
    val change : ('k, 'v) t -> 'k key -> ('v option -> 'v option) -> ('k,'v) t

    

    (** returns the value bound to the given key, raising [Not_found] if none
        such exists *)
    val find_exn: ('k, 'v) t -> 'k key -> 'v

    val find: ('k, 'v) t -> 'k key -> 'v option

    (** returns a new map with any binding for the key in question removed *)
    val remove: ('k, 'v) t -> 'k key -> ('k, 'v) t

    (** [mem key map] tests whether [map] contains a binding for [key] *)
    val mem: ('k, 'v) t -> 'k key -> bool

    (** iterator for map *)
    val iter: f:(key:'k key -> data:'v -> unit) -> ('k, 'v) t -> unit

    (** returns new map with bound values replaced by f applied to the bound values *)
    val map: f:('v1 -> 'v2) -> ('k, 'v1) t -> ('k, 'v2) t

    (** like [map], but function takes both key and data as arguments *)
    val mapi: f:(key:'k key -> data:'v1 -> 'v2) -> ('k, 'v1) t -> ('k, 'v2) t

    (** folds over keys and data in map *)
    val fold:
      f:(key:'k key -> data:'v -> 'a -> 'a) -> ('k, 'v) t -> init:'a -> 'a

    (** folds over keys and data in map in reverse order *)
    val fold_right:
      f:(key:'k key -> data:'v -> 'a -> 'a) -> ('k, 'v) t -> init:'a -> 'a

    (** filter for map *)
    val filter: f:(key:'k key -> data:'v -> bool) -> ('k, 'v) t -> ('k, 'v) t

    (** returns new map with bound values filtered by f applied to the bound
        values *)
    val filter_map: f:('v1 -> 'v2 option) -> ('k, 'v1) t -> ('k, 'v2) t

    (** like [filter_map], but function takes both key and data as arguments*)
    val filter_mapi:
      f:(key:'k key -> data:'v1 -> 'v2 option) -> ('k, 'v1) t -> ('k, 'v2) t

    (** Total ordering between maps.  The first argument is a total ordering
        used to compare data associated with equal keys in the two maps. *)
    val compare: ('v -> 'v -> int) -> ('k, 'v) t -> ('k, 'v) t -> int

    (** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are
        equal, that is, contain equal keys and associate them with
        equal data.  [cmp] is the equality predicate used to compare
        the data associated with the keys. *)
    val equal: ('v -> 'v -> bool) -> ('k, 'v) t -> ('k, 'v) t -> bool

    (** returns list of keys in map *)
    val keys: ('k, 'v) t -> 'k key list

    (** equivalent to [mem] *)
    val has_key: ('k, 'v) t -> 'k key -> bool

    (** returns list of data in map *)
    val data: ('k, 'v) t -> 'v list

    (** creates map from association list with unique keys *)
    val of_alist:
      ('k key * 'v) list -> [ `Ok of ('k, 'v) t | `Duplicate_key of 'k key ]

    (** creates map from association list with unique keys.  Raises Failure if
        duplicate 'a keys are found. *)
    val of_alist_exn: ('k key * 'v) list -> ('k, 'v) t

    (** creates map from association list with possibly repeated keys. *)
    val of_alist_multi: ('k key * 'v) list -> ('k, 'v list) t

    (** creates association list from map.  No guarantee about order. *)
    val to_alist: ('k, 'v) t -> ('k key * 'v) list

    (** {6 Additional operations on maps} *)

    
    (** combines an association list into a map, folding together the bound
        values (for experts only) *)
    val combine_alist:
      ('k key * 'v1) list -> init:'v2 -> f:('v1 -> 'v2 -> 'v2) -> ('k, 'v2) t

    
    (** merges two maps *)
    val merge:
      f:(key:'k key -> 'v1 option -> 'v2 option -> 'v3 option)
      -> ('k, 'v1) t -> ('k, 'v2) t -> ('k, 'v3) t

    (** [min_elt map] @return Some [(key, data)] pair corresponding to the
        minimum key in [map], None if empty. *)
    val min_elt : ('k, 'v) t -> ('k key * 'v) option

    (** [min_elt_exn map] @return the [(key, data)] pair corresponding to the
        minimum key in [map], raises [Not_found] if [map] is empty. *)
    val min_elt_exn : ('k, 'v) t -> 'k key * 'v

    (** [max_elt map] @return Some [(key, data)] pair corresponding to the
        maximum key in [map], and None if [map] is empty. *)
    val max_elt : ('k, 'v) t -> ('k key * 'v) option

    (** [max_elt_exn map] @return the [(key, data)] pair corresponding to the
        maximum key in [map], raises an exception if [map] is empty. *)
    val max_elt_exn : ('k, 'v) t -> 'k key * 'v

    (** same semantics as similar functions in List *)
    
    val for_all : f:('v -> bool) -> ('k, 'v) t -> bool
    val exists  : f:('v -> bool) -> ('k, 'v) t -> bool

    val fold_range_inclusive :
      ('k, 'v) t -> min:'k key -> max:'k key -> init:'a
      -> f:(key:'k key -> data:'v -> 'a -> 'a) -> 'a
    val range_to_alist : ('k, 'v) t -> min:'k key -> max:'k key -> ('k key * 'v) list

    val prev_key : ('k, 'v) t -> 'k key -> 'k key option
    val next_key : ('k, 'v) t -> 'k key -> 'k key option
    val rank : ('k, 'v) t -> 'k key -> int option
  end
end

module type S = sig
  type key
  type +'a t
  module T : Types with type 'a key = key with type ('a, +'b) t = 'b t

  include Sexpable.S1 with type +'a sexpable = 'a t

  val empty: 'a t
  val singleton: key -> 'a -> 'a t
  val is_empty: 'a t -> bool
  val cardinal : 'a t -> int
  val add: key:key -> data:'a -> 'a t -> 'a t
  val add_multi: key:key -> data:'a -> 'a list t -> 'a list t
  val change : 'a t -> key -> ('a option -> 'a option) -> 'a t
  val find_exn: 'a t -> key -> 'a
  val find: 'a t -> key -> 'a option
  val remove: 'a t -> key -> 'a t
  val mem: 'a t -> key -> bool
  val iter: f:(key:key -> data:'a -> unit) -> 'a t -> unit
  val map: f:('a -> 'b) -> 'a t -> 'b t
  val mapi: f:(key:key -> data:'a -> 'b) -> 'a t -> 'b t
  val fold: f:(key:key -> data:'a -> 'b -> 'b) -> 'a t -> init:'b -> 'b
  val fold_right: f:(key:key -> data:'a -> 'b -> 'b) -> 'a t -> init:'b -> 'b

  val filter: f:(key:key -> data:'a -> bool) -> 'a t -> 'a t
  val filter_map: f:('a -> 'b option) -> 'a t -> 'b t
  val filter_mapi: f:(key:key -> data:'a -> 'b option) -> 'a t -> 'b t
  val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val keys: 'a t -> key list
  val has_key: 'a t -> key -> bool
  val data: 'a t -> 'a list
  val of_alist: (key * 'a) list -> [ `Ok of 'a t | `Duplicate_key of key ]
  val of_alist_exn: (key * 'a) list -> 'a t
  val of_alist_multi: (key * 'a) list -> 'a list t
  val to_alist: 'a t -> (key * 'a) list
  val combine_alist: (key * 'b) list -> init:'a -> f:('b -> 'a -> 'a)
    -> 'a t
  val merge: f:(key:key -> 'data1 option -> 'data2 option -> 'data3 option) ->
    'data1 t -> 'data2 t -> 'data3 t
  val min_elt : 'data t -> (key * 'data) option
  val min_elt_exn : 'data t -> key * 'data
  val max_elt : 'data t -> (key * 'data) option
  val max_elt_exn : 'data t -> key * 'data
  val for_all : f:('data -> bool) -> 'data t -> bool
  val exists : f:('data -> bool) -> 'data t -> bool
  val fold_range_inclusive :
    'data t -> min:key -> max:key -> init:'a
    -> f:(key:key -> data:'data -> 'a -> 'a) -> 'a
  val range_to_alist : 'data t -> min:key -> max:key -> (key * 'data) list
  val prev_key : 'data t -> key -> key option
  val next_key : 'data t -> key -> key option
  val rank : 'a t -> key -> int option
end

module type S2 = sig
  type ('k, +'v) t
  module T : Types with type 'a key = 'a with type ('a, +'b) t = ('a, 'b) t

  include Binable.S2 with type ('a, +'b) binable = ('a, 'b) t
  include Sexpable.S2 with type ('a, +'b) sexpable = ('a, 'b) t

  val empty: ('k, 'v) t
  val singleton: 'k -> 'v -> ('k, 'v) t
  val is_empty: ('k, 'v) t -> bool
  val cardinal : ('k, 'v) t -> int
  val add: key:'k -> data:'v -> ('k, 'v) t -> ('k, 'v) t
  val add_multi: key:'k -> data:'v -> ('k, 'v list) t -> ('k, 'v list) t
  val change : ('k, 'v) t -> 'k -> ('v option -> 'v option) -> ('k,'v) t
  val find_exn: ('k, 'v) t -> 'k -> 'v
  val find: ('k, 'v) t -> 'k -> 'v option
  val remove: ('k, 'v) t -> 'k -> ('k, 'v) t
  val mem: ('k, 'v) t -> 'k -> bool
  val iter: f:(key:'k -> data:'v -> unit) -> ('k, 'v) t -> unit
  val map: f:('v -> 'b) -> ('k, 'v) t -> ('k, 'b) t
  val mapi: f:(key:'k -> data:'v -> 'b) -> ('k, 'v) t -> ('k, 'b) t
  val fold: f:(key:'k -> data:'v -> 'b -> 'b) -> ('k, 'v) t -> init:'b -> 'b
  val fold_right: f:(key:'k -> data:'v -> 'b -> 'b) -> ('k, 'v) t -> init:'b -> 'b
  val filter: f:(key:'k -> data:'v -> bool) -> ('k, 'v) t -> ('k, 'v) t
  val filter_map: f:('v -> 'b option) -> ('k, 'v) t -> ('k, 'b) t
  val filter_mapi: f:(key:'k -> data:'v -> 'b option) -> ('k, 'v) t -> ('k, 'b) t
  val compare: ('v -> 'v -> int) -> ('k, 'v) t -> ('k, 'v) t -> int
  val equal: ('v -> 'v -> bool) -> ('k, 'v) t -> ('k, 'v) t -> bool
  val keys: ('k, 'v) t -> 'k list
  val has_key: ('k, 'v) t -> 'k -> bool
  val data: ('k, 'v) t -> 'v list
  val of_alist: ('k * 'v) list -> [ `Ok of ('k, 'v) t | `Duplicate_key of 'k ]
  val of_alist_exn: ('k * 'v) list -> ('k, 'v) t
  val of_alist_multi: ('k * 'v) list -> ('k, 'v list) t
  val to_alist: ('k, 'v) t -> ('k * 'v) list
  val combine_alist:
    ('k * 'v1) list -> init:'v2 -> f:('v1 -> 'v2 -> 'v2) -> ('k, 'v2) t
  val merge: f:(key:'k -> 'data1 option -> 'data2 option -> 'data3 option) ->
    ('k, 'data1) t -> ('k, 'data2) t -> ('k, 'data3) t
  val min_elt : ('k, 'data) t -> ('k * 'data) option
  val min_elt_exn : ('k, 'data) t -> 'k * 'data
  val max_elt : ('k, 'data) t -> ('k * 'data) option
  val max_elt_exn : ('k, 'data) t -> 'k * 'data
  val for_all : f:('data -> bool) -> ('k, 'data) t -> bool
  val exists : f:('data -> bool) -> ('k, 'data) t -> bool
  val fold_range_inclusive :
    ('k, 'data) t -> min:'k -> max:'k -> init:'a
    -> f:(key:'k -> data:'data -> 'a -> 'a) -> 'a
  val range_to_alist : ('k, 'data) t -> min:'k -> max:'k -> ('k * 'data) list

  val next_key : ('k, 'data) t -> 'k -> 'k option
  val prev_key : ('k, 'data) t -> 'k -> 'k option
  val rank : ('k, 'data) t -> 'k -> int option
end

module type Gen = sig
  module T : Types
  include Gen(T).S
end

(* Check that S and S2 are instances of Gen *)
module Check_S (M : S) = (M : Gen(M.T).S)
module Check_S2 (M : S2) = (M : Gen(M.T).S)
