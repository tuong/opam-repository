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

module Hashtbl = Core_hashtbl
let _hash = Core_hashtbl.hash           (* causes omake to work *shrug* *)
module List = StdLabels.List
open Sexplib
open Sexplib.Conv

type 'a t = ('a, unit) Hashtbl.t
type 'a hash_set = 'a t

let clear = Hashtbl.clear
let length = Hashtbl.length
let mem = Hashtbl.mem
let is_empty t = Hashtbl.length t = 0

let add t k = Hashtbl.replace t ~key:k ~data:()

let strict_add t k =
  if mem t k then failwith "Hash_set.strict_add"
  else Hashtbl.replace t ~key:k ~data:()

let remove = Hashtbl.remove
let strict_remove t k =
  if mem t k then remove t k else failwith "Hash_set.strict_remove"

let fold ~f ~init t = Hashtbl.fold t ~init ~f:(fun ~key ~data:() acc -> f acc key)
let iter ~f t = Hashtbl.iter t ~f:(fun ~key ~data:() -> f key)
let to_list = Hashtbl.keys

let equal t1 t2 = Hashtbl.equal t1 t2 (fun () () -> true)

let copy t = Hashtbl.copy t

let t_of_sexp_internal create k_of_sexp sexp =
  let t = create () in
  let keys = list_of_sexp k_of_sexp sexp in
  List.iter keys ~f:(fun k -> strict_add t k);
  t

let of_list_internal create l =
  let t = create ~size:(List.length l) () in
  List.iter l ~f:(fun k -> add t k);
  t

module Poly = struct

  type 'a t = 'a hash_set
  type 'a sexpable = 'a t

  let create = Hashtbl.Poly.create

  let of_list l = of_list_internal (fun ~size () -> create ~size ()) l

  let sexp_of_t sexp_of_k t = sexp_of_list sexp_of_k (to_list t)

  let t_of_sexp k_of_sexp sexp = t_of_sexp_internal create k_of_sexp sexp

end

module Make (H : Hashtbl.Key) = struct
  module T = Hashtbl.Make (H)

  type sexpable = H.t t
  type elem = H.t

  type 'a z = 'a t
  type t = H.t z

  let create = T.create

  let of_list l = of_list_internal (fun ~size () -> create ~size ()) l

  let t_of_sexp sexp = t_of_sexp_internal T.create H.t_of_sexp sexp

  let sexp_of_t set = Poly.sexp_of_t H.sexp_of_t set

end

module Make_binable (H : sig
  include Hashtbl.Key
  include Binable.S with type binable = t
end) = struct
  include Make (H)

  type hash_set = t

  module Make_iterable_binable_spec = struct
    type t = hash_set
    type el = H.t with bin_io
    type acc = t
    let module_name = Some "Core.Hash_set"
    let length = length
    let iter = iter
    let init size = create ~size ()
    let insert acc v _i = add acc v; acc
    let finish t = t
  end

  include Bin_prot.Utils.Make_iterable_binable (Make_iterable_binable_spec)
end
