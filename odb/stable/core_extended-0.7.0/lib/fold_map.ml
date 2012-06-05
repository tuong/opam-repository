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

(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)

open Core.Std
module Map_intf = Core.Core_map_intf

(* We use functors to factor out the common signature parts between the gen and
   specialised implementations. We expend them in the mli to keep things
   readable. *)
module type Fold_map_funs =
sig
  type 'a _in_value
  type 'a _out_value
  type ('a,'b) _t
  val empty     : (_,_) _t
  val singleton : 'a -> 'b _in_value -> ('a,'b) _t
  val is_empty  : (_,_) _t -> bool
  val cardinal  : (_,_) _t -> int
  val add       : key:'a
    -> data:'b _in_value
    -> ('a,'b) _t
    -> ('a,'b) _t
  val find      : ('a,'b) _t -> 'a -> 'b _out_value
  val remove    : ('a,'b) _t -> 'a -> ('a,'b) _t
  val set       :
    key:'a
    -> data:'b _out_value
    -> ('a,'b) _t
    -> ('a,'b) _t
  val mem       : ('a,_) _t -> 'a -> bool
  val iter      :
    f:(key:'a -> data:'b _out_value -> unit)
    -> ('a,'b) _t
    -> unit
  val fold      :
    f:(key:'a -> data:'b _out_value -> 'c -> 'c)
    -> ('a,'b) _t
    -> init:'c
    -> 'c
  val filter    :
    f:(key:'a -> data:'b _out_value -> bool)
    -> ('a,'b) _t
    -> ('a,'b) _t
  val keys      : ('a,_) _t -> 'a list
  val data      : (_,'b) _t -> 'b _out_value list
  val to_alist  : ('a,'b) _t -> ('a * 'b _out_value) list
  val of_list   : ('a * 'b _in_value) list -> ('a,'b) _t
  val for_all   : f:('b _out_value -> bool) -> (_,'b) _t -> bool
  val exists    : f:('b _out_value -> bool) -> (_,'b) _t -> bool
  val to_map    : ('a,'b) _t -> ('a,'b _out_value) Map.t
  val of_map    : ('a,'b _out_value) Map.t -> ('a,'b) _t
end

module type Foldable_gen = sig
  type 'a t
  type 'a data
  val init : _ t
  val f : 'a t -> 'a data -> 'a t
end


(* implementation *)
module Make_fun (Fold : Foldable_gen) : Fold_map_funs
  with type 'a _in_value = 'a Fold.data
  and  type 'a _out_value = 'a Fold.t
  and  type ('a,'b) _t = ('a,'b Fold.t) Map.t
  =
struct
  include (Map : Map_intf.Gen(Map.T).S)
  type 'a _in_value = 'a Fold.data
  type 'a _out_value = 'a Fold.t
  type ('a,'b) _t = ('a,'b Fold.t) Map.t

  let to_map = ident
  let of_map = ident

  let singleton key in_value = Map.singleton key (Fold.f Fold.init in_value)
  let find t key =
    match Map.find t key with
    | None -> Fold.init
    | Some v -> v

  let add ~key ~data t =
    Map.add ~key ~data:(Fold.f (find t key) data) t

  let set = Map.add

  let of_list l = List.fold l ~init:empty ~f:(fun t (key,data) ->
    add t ~key ~data)
end

(* argument to functor: maintains per-key state *)
module type Foldable = sig
  type t
  type data
  val init : t
  val f : t -> data -> t
end

(* A map where the type of values added can be different from the type of values gotten out. *)
module type S = sig
  type in_value
  type out_value
  type 'a t = private (('a,out_value) Map.t)

  include (Fold_map_funs with type 'a _in_value  = in_value
                         and  type 'a _out_value = out_value
                         and  type ('a,'b) _t     = 'a t)
end

module Make (Fold : Foldable) = struct
  type in_value = Fold.data
  type out_value = Fold.t               (* with sexp *)
  type 'a t = ('a,Fold.t) Map.t (* with sexp *)

  include (Make_fun(struct
                      type 'a t = Fold.t
                      type 'a data = Fold.data
                      let init = Fold.init
                      let f = Fold.f
                    end))
end

module type Foldable_sexpable = sig
  include Foldable
  include Sexpable with type sexpable = t
end

module type S_sexpable = sig
  include S
  include Sexpable.S1 with type 'key sexpable = 'key t
end


module Make_sexpable (Fold : Foldable_sexpable)
  = struct
    include Make (Fold)
    type 'key sexpable = 'key t
    let t_of_sexp key_of_sexp = Map.t_of_sexp key_of_sexp Fold.t_of_sexp
    let sexp_of_t sexp_of_key = Map.sexp_of_t sexp_of_key Fold.sexp_of_t
  end

module type Foldable2 = sig
  type 'a t
  val init : _ t
  val f : 'a t -> 'a -> 'a t
end

module type S2 = sig
  type 'a out_value
  type ('a,'b) t = private (('a,'b out_value) Map.t)

  include (Fold_map_funs with type ('a) _in_value  = 'a
                         and  type ('a) _out_value = 'a out_value
                         and  type ('a,'b) _t     = ('a,'b) t)
end

module Make2 (Fold : Foldable2) = struct
  type 'a in_value = 'a
  type 'a out_value = 'a Fold.t
  type ('a,'b) t = ('a,'b Fold.t) Map.t

  include (Make_fun(struct include Fold type 'a data = 'a end))
end

module type Foldable2_sexpable = sig
  include Foldable2
  include Sexpable.S1 with type 'a sexpable = 'a t
end

module type S2_sexpable = sig
  include S2
  include Sexpable.S2 with type ('a,'b) sexpable = ('a,'b) t
end

module Make2_sexpable (Fold : Foldable2_sexpable) = struct
  type 'a in_value = 'a
  type 'a out_value = 'a Fold.t               (* with sexp *)
  type ('a,'b) t = ('a,'b Fold.t) Map.t with sexp
  type ('a,'b) sexpable = ('a,'b) t

  include (Make_fun((struct include Fold type 'a data = 'a end)))
end



module Cons =
  Make2_sexpable
    (struct
       type 'a data = 'a
       type 'a t = 'a list with sexp
       type 'a sexpable = 'a t
       let init = []
       let f list x = x :: list
     end)

(* Fold for adding, e.g. symbol positions *)
module Add = Make_sexpable
(struct
  type data = int
  include Int
  let init = 0
  let f = (+)
 end)

module Multiply = Make_sexpable
(struct
  type data = int
  include Int
  let init = 1
  let f = ( * )
end)
