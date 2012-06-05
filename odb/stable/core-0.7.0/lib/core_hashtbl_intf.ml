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

open Sexplib


type 'a hashable = {
  hash: 'a -> int;
  compare: 'a -> 'a -> int;
}

module type Key = sig
  type t

  val compare : t -> t -> int
  val hash : t -> int

  include Sexpable.S with type sexpable = t
end

module type T0 = sig type t end
module type T1 = sig type 'a t end
module type T2 = sig type ('a, 'b) t end

module Key_poly = struct
  type 'a t = 'a
end

module Access_sig (T : T2) (Key : T1) = struct
  open T
  module type S = sig
    val clear : (_, _) t -> unit
    val copy : ('a, 'b) t -> ('a, 'b) t
    val invariant : (_, _) t -> unit
    val fold :
      ('a, 'b) t -> init:'c -> f:(key:'a Key.t -> data:'b -> 'c -> 'c) -> 'c
    val iter : ('a, 'b) t -> f:(key:'a Key.t -> data:'b -> unit) -> unit
    val length : (_, _) t -> int
    val is_empty : (_, _) t -> bool
    val mem : ('a, _) t -> 'a Key.t -> bool
    val remove : ('a, _) t -> 'a Key.t -> unit

    (* [remove_one t key] if [key] is present in the table, and [data] is has at
       least two elements then replace [key] with [List.tl data], otherwise
       remove [key] *)
    val remove_one : ('a, _ list) t -> 'a Key.t -> unit

    
    
    val replace : ('a, 'b) t -> key:'a Key.t -> data:'b -> unit

    (** [change t key f] updates the given table by changing the value stored under [key]
        according to [f], just like [Map.change] (see that for example). *)
    val change : ('a, 'b) t -> 'a Key.t -> ('b option -> 'b option) -> unit

    
    (* [add_multi t ~key ~data] if [key] is present in the table then cons
       [data] on the list, otherwise add [key] with a single element list. *)
    val add_multi : ('a, 'b list) t -> key:'a Key.t -> data:'b -> unit

    (** [map t f] returns new table with bound values replaced by
        [f] applied to the bound values *)
    val map : ('a, 'b) t -> f:('b -> 'c) -> ('a, 'c) t

    (** like [map], but function takes both key and data as arguments *)
    val mapi : ('a, 'b) t -> f:(key:'a Key.t -> data:'b -> 'c) -> ('a, 'c) t

    (** returns new map with bound values filtered by f applied to the bound
        values *)
    val filter_map : ('a, 'b) t -> f:('b -> 'c option) -> ('a, 'c) t

    (** like [filter_map], but function takes both key and data as arguments*)
    val filter_mapi : ('a, 'b) t -> f:(key:'a Key.t -> data:'b -> 'c option) -> ('a, 'c) t

    (** [find_or_add t k ~default] returns the data associated with key k if it
        is in the table t, otherwise it lets d = default() and adds it to the
        table. *)
    val find_or_add : ('a, 'b) t -> 'a Key.t -> default:(unit -> 'b) -> 'b

    (** [find t k] returns Some (the current binding) of k in t, or None if no
        such binding exists *)
    val find : ('a, 'b) t -> 'a Key.t -> 'b option

    (** [find_exn t k] returns the current binding of k in t, or raises Not_found
        if no such binding exists.*)
    val find_exn : ('a, 'b) t -> 'a Key.t -> 'b
    (** [iter_vals t ~f] is like iter, except it only supplies the value to f,
        not the key. *)
    val iter_vals : ('a, 'b) t -> f:('b -> unit) -> unit

    
    (** Merge two hashtables.

        The result of [merge f h1 h2] has as keys the set of all [k] in the
        union of the sets of keys of [h1] and [h2] for which [d(k)] is not
        None, where:

        d(k) =
        - f ~key:k (Some d1) None
        if the *most recent* binding of [k] in [h1] is to d1,
        and [h2] does not map [k];

        - f ~key:k None (Some d2)
        if the *most recent* binding of [k] in [h2] is to d2,
        and [h1] does not map [k];

        - f ~key:k (Some d1) (Some d2)
        otherwise, where the *most recent* binding of [k] in [h1]
        is to [d1] and the *most recent* binding of [k] in [h2]
        is to [d2].

        Each key [k] is mapped to a single piece of data x, where
        [d(k)] = Some x.
    *)
    
    val merge :
      f:(key:'k Key.t -> 'a option -> 'b option -> 'c option)
      -> ('k, 'a) t
      -> ('k, 'b) t
      -> ('k, 'c) t

    (** Merge one hashtable into another.

        After [merge_into f src dst], for every [key] in [src], [key] will be
        re-mapped in [dst] to [v] if [f ~key d1 (find dst key) = Some v].
    *)
    val merge_into:
      f:(key:'a Key.t -> 'b -> 'b option -> 'b option)
      -> src:('a, 'b) t
      -> dst:('a, 'b) t
      -> unit

    (** Returns the list of all unique keys for given hashtable. Keys bound
        multiple times will not be returned twice. *)
    val keys : ('a, 'b) t -> 'a Key.t list
    (** Returns the list of all data for given hashtable. *)
    val data : ('a, 'b) t -> 'b list
    (** [filter_inplace t ~f] removes all the elements from [t] that don't
     * satisfy [f].
     *)
    val filter_inplace : ('a, 'b) t -> f:('b -> bool) -> unit
    val filteri_inplace : ('a, 'b) t -> f:('a Key.t -> 'b -> bool) -> unit

    val equal : ('a, 'b) t -> ('a, 'b) t -> ('b -> 'b -> bool) -> bool

    (** Returns the list of all (key,data) pairs for given hashtable. *)
    val to_alist : ('a, 'b) t -> ('a Key.t * 'b) list

    val incr : ?by:int -> ('a, int) t -> 'a Key.t -> unit
  end
end

type 'a with_options =
  ?growth_allowed:bool          
  -> ?size:int   (* initial size -- default 128 *)
  -> 'a

module Create_sig (T : T2) (Key : T1) = struct
  open T

  module type S = sig

    val create : (unit -> ('a, 'b) t) with_options

    val of_alist :
      (('a Key.t * 'b) list
       -> [ `Ok of ('a, 'b) t
          | `Duplicate_key of 'a Key.t
          ]) with_options

    
    val of_alist_exn : (('a Key.t * 'b) list -> ('a, 'b) t) with_options

    val of_alist_multi : (('a Key.t * 'b) list -> ('a, 'b list) t) with_options

    

    

    (* create_mapped get_key get_data [x1,...,xn] =
         of_alist [get_key x1, get_data x1; ...; get_key xn, get_data xn] *)
    val create_mapped :
      (get_key:('r -> 'a Key.t)
       -> get_data:('r -> 'b)
       -> 'r list
       -> ('a, 'b) t) with_options

    (* create_with_key ~get_key [x1,...,xn] =
         of_alist [get_key x1, x1; ...; get_key xn, xn] *)
    val create_with_key :
      (get_key:('r -> 'a Key.t)
       -> 'r list
       -> ('a, 'r) t) with_options

    val group :
      (get_key:('r -> 'a Key.t)
       -> get_data:('r -> 'b)
       -> combine:('b -> 'b -> 'b)
       -> 'r list
       -> ('a, 'b) t) with_options
  end
end

module Monomorphic (T : T2) (Key : T0) = struct
  module T = struct
    type ('a, 'b) t = (Key.t, 'b) T.t
  end
  module Key1 = struct
    type 'a t = Key.t
  end
  module type S = sig
    module Key : T0 with type t = Key.t
    val hashable : Key.t hashable
    type 'a t = (Key.t, 'a) T.t
    
    include Access_sig (T) (Key1).S
    include Create_sig (T) (Key1).S
    include Sexpable.S1 with type 'a sexpable = 'a t
  end
end
