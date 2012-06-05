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
open Core_map_intf
open With_return

module List = Core_list

module type Key = Key
module type S = S

module type S_binable = sig
  include S
  include Binable.S1 with type 'a binable = 'a t
end



type ('k, +'v) tree =
  | Empty
  | Node of ('k, 'v) tree * 'k * 'v * ('k, 'v) tree * int

module Raw_impl
  (Key : sig
    (* The type [t] is unary because we need to use it both for unary and
     * nullary key types.  See the two calls to [Raw_impl] below.
     *)
    type 'a t
    val compare : 'a t -> 'a t -> int
  end) = struct

  module T = struct
    type 'k key = 'k Key.t
    type ('k, +'v) t = ('k Key.t, 'v) tree
  end

  let height = function
      Empty -> 0
    | Node(_,_,_,_,h) -> h

  let create l x d r =
    let hl = height l and hr = height r in
    Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

  let singleton key data = create Empty key data Empty;;

  let bal l x d r =
    let hl = height l in
    let hr = height r in
    if hl > hr + 2 then begin
      match l with
        Empty -> invalid_arg "Map.bal"
      | Node(ll, lv, ld, lr, _) ->
          if height ll >= height lr then
            create ll lv ld (create lr x d r)
          else begin
            match lr with
              Empty -> invalid_arg "Map.bal"
            | Node(lrl, lrv, lrd, lrr, _)->
                create (create ll lv ld lrl) lrv lrd (create lrr x d r)
          end
    end else if hr > hl + 2 then begin
      match r with
        Empty -> invalid_arg "Map.bal"
      | Node(rl, rv, rd, rr, _) ->
          if height rr >= height rl then
            create (create l x d rl) rv rd rr
          else begin
            match rl with
              Empty -> invalid_arg "Map.bal"
            | Node(rll, rlv, rld, rlr, _) ->
                create (create l x d rll) rlv rld (create rlr rv rd rr)
          end
    end else
      Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

  let empty = Empty

  let is_empty = function Empty -> true | _ -> false

  let rec add ~key:x ~data = function
      Empty ->
        Node(Empty, x, data, Empty, 1)
    | Node(l, v, d, r, h) ->
        let c = Key.compare x v in
        if c = 0 then
          Node(l, x, data, r, h)
        else if c < 0 then
          bal (add ~key:x ~data l) v d r
        else
          bal l v d (add ~key:x ~data r)

  let rec find t x =
    match t with
    | Empty ->
        None
    | Node(l, v, d, r, _) ->
        let c = Key.compare x v in
        if c = 0 then Some d
        else find (if c < 0 then l else r) x

  let add_multi ~key ~data t =
    match find t key with
    | None -> add ~key ~data:[data] t
    | Some l -> add ~key ~data:(data :: l) t

  let rec find_exn t x =
    match find t x with
    | None ->
        raise Not_found
    | Some data -> data

  let mem t x = Option.is_some (find t x)

  let rec min_elt = function
    | Empty -> None
    | Node (Empty, k, d, _, _) -> Some (k, d)
    | Node (l, _, _, _, _) -> min_elt l
  ;;

  let rec min_elt_exn t =
    match min_elt t with
    
    | None -> raise Not_found
    | Some v -> v
  ;;

  let rec max_elt = function
    | Empty -> None
    | Node (_, k, d, Empty, _) -> Some (k, d)
    | Node (_, _, _, r, _) -> max_elt r
  ;;
  let rec max_elt_exn t =
    match max_elt t with
    
    | None -> raise Not_found
    | Some v -> v
  ;;

  let rec remove_min_elt t =
    match t with
      Empty -> invalid_arg "Map.remove_min_elt"
    | Node(Empty, _, _, r, _) -> r
    | Node(l, x, d, r, _) -> bal (remove_min_elt l) x d r

  (* assumes that min <= max in the ordering given by Key.compare *)
  let rec fold_range_inclusive t ~min ~max ~init ~f =
    match t with
    | Empty -> init
    | Node (l, k, d, r, _) ->
      let c_min = Key.compare k min in
      if c_min < 0 then
        (* if k < min, then this node and its left branch are outside our range *)
        fold_range_inclusive r ~min ~max ~init ~f
      else if c_min = 0 then
        (* if k = min, then this node's left branch is outside our range *)
        fold_range_inclusive r ~min ~max ~init:(f ~key:k ~data:d init) ~f
      else (* k > min *)
        begin
          let z = fold_range_inclusive l ~min ~max ~init ~f in
          let c_max = Key.compare k max in
          (* if k > max, we're done *)
          if c_max > 0 then z
          else
            let z = f ~key:k ~data:d z in
            (* if k = max, then we fold in this one last value and we're done *)
            if c_max = 0 then z
            else fold_range_inclusive r ~min ~max ~init:z ~f
        end

  let range_to_alist t ~min ~max =
    List.rev
      (fold_range_inclusive t ~min ~max ~init:[] ~f:(fun ~key ~data l -> (key,data)::l))

  
  let merge t1 t2 =
    match (t1, t2) with
      (Empty, t) -> t
    | (t, Empty) -> t
    | (_, _) ->
        let (x, d) = min_elt_exn t2 in
        bal t1 x d (remove_min_elt t2)

  let rec remove t x =
    match t with
    | Empty ->
        Empty
    | Node(l, v, d, r, _) ->
        let c = Key.compare x v in
        if c = 0 then
          merge l r
        else if c < 0 then
          bal (remove l x) v d r
        else
          bal l v d (remove r x)

  let rec iter ~f = function
      Empty -> ()
    | Node(l, v, d, r, _) ->
        iter ~f l; f ~key:v ~data:d; iter ~f r

  let rec map ~f = function
      Empty               -> Empty
    | Node(l, v, d, r, h) ->
        let l' = map ~f l in
        let d' = f d in
        let r' = map ~f r in
        Node(l', v, d', r', h)

  let rec mapi ~f = function
      Empty               -> Empty
    | Node(l, v, d, r, h) ->
        let l' = mapi ~f l in
        let d' = f ~key:v ~data:d in
        let r' = mapi ~f r in
        Node(l', v, d', r', h)

  let rec fold ~f t ~init:accu =
    match t with
      Empty -> accu
    | Node(l, v, d, r, _) ->
        fold ~f r ~init:(f ~key:v ~data:d (fold ~f l ~init:accu))

  let rec fold_right ~f t ~init:accu =
    match t with
      Empty -> accu
    | Node(l, v, d, r, _) ->
        fold_right ~f l ~init:(f ~key:v ~data:d (fold_right ~f r ~init:accu))

  let filter ~f t =
    fold ~init:Empty t ~f:(fun ~key ~data accu ->
      if f ~key ~data then add ~key ~data accu else accu
    )
  ;;

  let filter_map ~f t =
    fold ~init:Empty t ~f:(fun ~key ~data accu ->
      match f data with
      | None -> accu
      | Some b -> add ~key ~data:b accu
    )
  ;;

  let filter_mapi ~f t =
    fold ~init:Empty t ~f:(fun ~key ~data accu ->
      match f ~key ~data with
      | None -> accu
      | Some b -> add ~key ~data:b accu
    )
  ;;

  module Enum = struct
    type ('k, 'v) t =
      | End
      | More of 'k Key.t * 'v * ('k Key.t, 'v) tree * ('k, 'v) t

    let rec cons t e =
      match t with
        Empty -> e
      | Node(l, v, d, r, _) -> cons l (More(v, d, r, e))
  end

  
  let equal cmp t1 t2 =
    let rec equal_aux e1 e2 =
      match (e1, e2) with
        (Enum.End, Enum.End) -> true
      | (Enum.End, _)  -> false
      | (_, Enum.End) -> false
      | (Enum.More(v1, d1, r1, e1), Enum.More(v2, d2, r2, e2)) ->
          Key.compare v1 v2 = 0 && cmp d1 d2 &&
      equal_aux (Enum.cons r1 e1) (Enum.cons r2 e2)
    in equal_aux (Enum.cons t1 Enum.End) (Enum.cons t2 Enum.End)

  let compare cmp t1 t2 =
    
    let rec compare_aux e1 e2 =
      match (e1, e2) with
        (Enum.End, Enum.End) -> 0
      | (Enum.End, _)  -> -1
      | (_, Enum.End) -> 1
      | (Enum.More(v1, d1, r1, e1), Enum.More(v2, d2, r2, e2)) ->
          let c = Key.compare v1 v2 in
          if c <> 0 then c else
            let c = cmp d1 d2 in
            if c <> 0 then c else
              compare_aux (Enum.cons r1 e1) (Enum.cons r2 e2)
    in compare_aux (Enum.cons t1 Enum.End) (Enum.cons t2 Enum.End)

  let rec cardinal = function
    | Empty -> 0
    | Node (l, _, _, r, _) -> cardinal l + cardinal r + 1

  let combine_alist alist ~init ~f =
    List.fold alist ~init:empty
      ~f:(fun accum (key, data) ->
        let prev_data =
          match find accum key with
          | None -> init
          | Some prev -> prev
        in
        let data = f data prev_data in
        add accum ~key ~data
      )

  let keys t = fold_right ~f:(fun ~key ~data:_ list -> key::list) t ~init:[]
  let has_key = mem
  let data t = fold_right ~f:(fun ~key:_ ~data list -> data::list) t ~init:[]

  let of_alist alist =
    with_return (fun r ->
      let map =
        List.fold alist ~init:empty ~f:(fun t (key,data) ->
          if mem t key then r.return (`Duplicate_key key)
          else add ~key ~data t)
      in
      `Ok map)

  let for_all ~f t =
    with_return (fun r ->
      iter t ~f:(fun ~key:_ ~data -> if not (f data) then r.return false);
      true)

  let exists ~f t =
    with_return (fun r ->
      iter t ~f:(fun ~key:_ ~data -> if f data then r.return true);
      false)

  let of_alist_exn alist =
    match of_alist alist with
    | `Ok x -> x
    | `Duplicate_key _ -> failwith "Map.of_alist_exn: duplicate key"
  ;;

  let of_alist_multi alist =
    combine_alist alist ~init:[] ~f:Core_list.cons
  ;;

  let to_alist t =
    fold_right t ~init:[] ~f:(fun ~key ~data x -> (key,data)::x)
  ;;

  
  let merge ~f t1 t2 =
    let all_keys =
      Core_list.dedup ~compare:Key.compare (Core_list.append (keys t1) (keys t2))
    in
    List.fold ~init:empty all_keys
      ~f:(fun t key ->
            match f ~key (find t1 key) (find t2 key) with
            | None -> t
            | Some data -> add ~key ~data t)

  let rec next_key t k =
    match t with
    | Empty -> None
    | Node (l, k', _, r, _) ->
      let c = Key.compare k' k in
      if c = 0 then Option.map ~f:fst (min_elt r)
      else if c < 0 then next_key r k
      else begin match next_key l k with
      | None -> Some k'
      | Some answer -> Some answer
      end

  let rec prev_key t k =
    match t with
    | Empty -> None
    | Node (l, k', _, r, _) ->
      let c = Key.compare k' k in
      if c = 0 then Option.map ~f:fst (max_elt l)
      else if c > 0 then prev_key l k
      else begin match prev_key r k with
      | None -> Some k'
      | Some answer -> Some answer
      end

  let rec rank t k =
    match t with
    | Empty -> None
    | Node (l, k', _, r, _) ->
      let c = Key.compare k' k in
      if c = 0
      then Some (cardinal l)
      else if c > 0
      then rank l k
      else Option.map (rank r k) ~f:(fun rank -> rank + 1 + (cardinal l))


  
  let change t key f =
    match f (find t key) with
    | None -> remove t key
    | Some data -> add t ~key ~data

  let t_of_sexp key_of_sexp value_of_sexp = function
    | Type.List lst ->
        let coll t = function
          | Type.List [k_sexp; v_sexp] ->
              let key = key_of_sexp k_sexp in
              let value = value_of_sexp v_sexp in
              if mem t key then Conv.of_sexp_error "Map.t_of_sexp: duplicate key" k_sexp
              else add ~key ~data:value t
          | sexp -> Conv.of_sexp_error "Map.t_of_sexp: tuple list needed" sexp
        in
        List.fold ~f:coll ~init:empty lst
    | sexp ->
        Conv.of_sexp_error "Map.t_of_sexp: list needed" sexp

  let sexp_of_t sexp_of_key sexp_of_value t =
    let f ~key ~data acc = Type.List [sexp_of_key key; sexp_of_value data] :: acc in
    Type.List (fold_right ~f t ~init:[])
end

module Make (Key : Key) = struct
  include Raw_impl (struct
    type 'a t = Key.t
    let compare = Key.compare
  end)

  type key = Key.t
  type +'v t = (Key.t, 'v) tree

  type +'a sexpable = 'a t
  let t_of_sexp a_of_sexp sexp = t_of_sexp Key.t_of_sexp a_of_sexp sexp
  let sexp_of_t sexp_of_a t = sexp_of_t Key.sexp_of_t sexp_of_a t
end

module Key = struct
  type 'a t = 'a
  let compare = Pervasives.compare
end

include Raw_impl (Key)

type ('a, +'b) t = ('a, 'b) tree
type ('a, +'b) sexpable = ('a, 'b) t


include Bin_prot.Utils.Make_iterable_binable2 (struct
  type ('a, 'b) t = ('a, 'b) tree
  type ('a, 'b) el = 'a * 'b with bin_io
  type ('a, 'b) acc = ('a , 'b) t
  let module_name = Some "Core.Core_map"
  let length = cardinal
  let iter ~f t = iter ~f:(fun ~key ~data -> f (key, data)) t
  let init _n = empty

  let insert acc (key, data) _i =
    if mem acc key then failwith "Map.bin_read_t_: duplicate element in map"
    else add ~key ~data acc

  let finish t = t
end)

module Make_binable (Key : sig
  include Key
  include Binable.S with type binable = t
end) = struct
  include Raw_impl (struct
    type 'a t = Key.t
    let compare = Key.compare
  end)

  type key = Key.t

  type +'v dummy = (Key.t, 'v) t with bin_io
  type +'v t = 'v dummy with bin_io
  type +'v binable = 'v t

  type +'v sexpable = 'v t
  let t_of_sexp v_of_sexp sexp = t_of_sexp Key.t_of_sexp v_of_sexp sexp
  let sexp_of_t sexp_of_v t = sexp_of_t Key.sexp_of_t sexp_of_v t
end
