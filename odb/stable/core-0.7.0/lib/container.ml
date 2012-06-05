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

 (* This file has generic signatures for container data structures, with standard
   functions (iter, fold, exists, for_all, ...) that one would expect to find in
   any container.  The idea is to include [Container.S0] or [Container.S1] in
   the signature for every container-like data structure (Array, List, String,
   ...) to ensure a consistent interface.
*)


type ('elt, 't) type_class = {
  length : 't -> int;
  is_empty : 't -> bool;
  iter : 't -> f:('elt -> unit) -> unit;
  (** [fold f a [b1; ...; bn]] is
      [f (... (f (f a b1) b2) ...) bn]. *)
  fold : 'accum. 't -> init:'accum -> f:('accum -> 'elt -> 'accum) -> 'accum;
  exists : 't -> f:('elt -> bool) -> bool;
  for_all : 't -> f:('elt -> bool) -> bool;
  find : 't -> f:('elt -> bool) -> 'elt option;
  to_list : 't -> 'elt list;
  to_array : 't -> 'elt array;
  (* compare : 't -> 't -> cmp:('elt -> 'elt -> int) -> int *)
}

(* Needed so that create can take a polymorphic function as argument. *)

type ('elt, 't) fold =
  { f : 'accum. 't -> init:'accum -> f:('accum -> 'elt -> 'accum) -> 'accum }

open With_return



let create ~fold ?length ?is_empty ?iter ?exists ?for_all ?find ?to_list ?to_array
    () =
  let fold = fold.f in
  let iter' t ~f = fold t ~init:() ~f:(fun () a -> f a) in
  (* like Option.value, but avoids cyclic dependency *)
  let uw opt default = match opt with None -> default | Some x -> x in
  let iter = uw iter iter' in
  let length' c = fold c ~init:0 ~f:(fun acc _ -> acc + 1) in
  let is_empty' c =
    with_return (fun r ->
      iter c ~f:(fun _ -> r.return false);
      true)
  in
  let exists' c ~f =
    with_return (fun r ->
      iter c ~f:(fun x -> if f x then r.return true);
      false)
  in
  let for_all' c ~f =
    with_return (fun r ->
      iter c ~f:(fun x -> if not (f x) then r.return false);
      true)
  in
  let find' c ~f =
    with_return (fun r ->
      iter c ~f:(fun x -> if f x then r.return (Some x));
      None)
  in
  let to_list' c =
    List.rev (fold c ~init:[] ~f:(fun acc x -> x :: acc))
  in
  let to_array' c = Array.of_list (to_list' c) in
  { iter     = iter;
    fold     = fold;
    length   = uw length   length'  ;
    is_empty = uw is_empty is_empty';
    exists   = uw exists   exists'  ;
    for_all  = uw for_all  for_all' ;
    find     = uw find     find'    ;
    to_list  = uw to_list  to_list' ;
    to_array = uw to_array to_array';
  }


(* Signature for monomorphic container, e.g., string *)
module type S0_noclass = sig
  type container
  type elt

  val mem : ?equal:(elt -> elt -> bool) -> container -> elt -> bool
  val length : container -> int
  val is_empty : container -> bool
  val iter : container -> f:(elt -> unit) -> unit
  val fold : container -> init:'accum -> f:('accum -> elt -> 'accum) -> 'accum
  val exists : container -> f:(elt -> bool) -> bool
  val for_all : container -> f:(elt -> bool) -> bool
  val find : container -> f:(elt -> bool) -> elt option
  val to_list : container -> elt list
  val to_array : container -> elt array
  (* val compare : container -> container -> cmp:(elt -> elt -> int) -> int *)
end

module type S0 = sig
  include S0_noclass
  val container : (elt, container) type_class
end

module type S0_phantom_noclass = sig
  type elt
  type 'a container
  val length : 'a container -> int
  val is_empty : 'a container -> bool
  val iter : 'a container -> f:(elt -> unit) -> unit
  val fold : 'a container -> init:'accum -> f:('accum -> elt -> 'accum) -> 'accum
  val exists : 'a container -> f:(elt -> bool) -> bool
  val for_all : 'a container -> f:(elt -> bool) -> bool
  val find : 'a container -> f:(elt -> bool) -> elt option
  val to_list : 'a container -> elt list
  val to_array : 'a container -> elt array
  (* val compare : 'a container -> 'a container -> cmp:(elt -> elt -> int) -> int *)
end

module type S0_phantom = sig
  include S0_phantom_noclass
  val container : (elt, 'a container) type_class
end


(* Signature for polymorphic container, e.g., 'a list or 'a array *)
module type S1_noclass = sig
  type 'a container
  val length : 'a container -> int
  val is_empty : 'a container -> bool
  val iter : 'a container -> f:('a -> unit) -> unit
  val fold : 'a container -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum
  val exists : 'a container -> f:('a -> bool) -> bool
  val for_all : 'a container -> f:('a -> bool) -> bool
  val find : 'a container -> f:('a -> bool) -> 'a option
  val to_list : 'a container -> 'a list
  val to_array : 'a container -> 'a array
  (* val compare : 'a container -> 'a container -> cmp:('a -> 'a -> int) -> int *)
end

module type S1 = sig
  include S1_noclass

  val container : ('elt, 'elt container) type_class
end

module type S1_phantom_noclass = sig
  type ('a, +'phantom) container
  val length : ('a, 'phantom) container -> int
  val is_empty : ('a, 'phantom) container -> bool
  val iter : ('a, 'phantom) container -> f:('a -> unit) -> unit
  val fold : ('a, 'phantom) container -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum
  val exists : ('a, 'phantom) container -> f:('a -> bool) -> bool
  val for_all : ('a, 'phantom) container -> f:('a -> bool) -> bool
  val find : ('a, 'phantom) container -> f:('a -> bool) -> 'a option
  val to_list : ('a, 'phantom) container -> 'a list
  val to_array : ('a, 'phantom) container -> 'a array
end

module type S1_phantom = sig
  include S1_phantom_noclass
  val container : ('elt, ('elt, 'phantom) container) type_class
end

(* The following functors exist as a consistency check among all the various
   [S?] interfaces.  They ensure that each particular [S?] is an instance of
   a more generic signature. *)
module Check (T : sig
  type 'a elt
  type 'a container
end) (M : sig
  open T
  val length : 'a container -> int
  val is_empty : 'a container -> bool
  val iter : 'a container -> f:('a elt -> unit) -> unit
  val fold : 'a container -> init:'accum -> f:('accum -> 'a elt -> 'accum) -> 'accum
  val exists : 'a container -> f:('a elt -> bool) -> bool
  val for_all : 'a container -> f:('a elt -> bool) -> bool
  val find : 'a container -> f:('a elt -> bool) -> 'a elt option
  
  val to_list : 'a container -> 'a elt list
  val to_array : 'a container -> 'a elt array
  (* val compare : 'a container -> 'a container -> cmp:('a elt -> 'a elt -> int) -> int *)
end) = struct end

module Check_S0 (M : S0) =
  Check (struct
    type 'a elt = M.elt
    type 'a container = M.container
  end) (M)

module Check_S0_phantom (M : S0_phantom) =
  Check (struct
    type 'a elt = M.elt
    type 'a container = 'a M.container
  end) (M)

module Check_S1 (M : S1) =
  Check (struct
    type 'a elt = 'a
    type 'a container = 'a M.container
  end)

type phantom

module Check_S1_phantom (M : S1_phantom) =
  Check (struct
    type 'a elt = 'a
    type 'a container = ('a, phantom) M.container
  end) (M)
