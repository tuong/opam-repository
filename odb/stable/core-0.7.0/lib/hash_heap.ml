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

(** A hash-heap is a combination of a heap and a hashtbl that supports
    constant time lookup, and log(n) time removal and replacement of
    elements in addition to the normal heap operations. *)

module Hashtbl = Core_hashtbl

module type Key = Hashtbl.Key

module type S = sig
  module Key : Key

  type 'a t

  val create : ?min_size:int -> ('a -> 'a -> int) -> 'a t
  val copy : 'a t -> 'a t
  val push : 'a t -> key:Key.t -> data:'a -> [`Ok | `Key_already_present]
  val push_exn : 'a t -> key:Key.t -> data:'a -> unit
  val replace : 'a t -> key:Key.t -> data:'a -> unit
  val remove : 'a t -> Key.t -> unit
  val mem : 'a t -> Key.t -> bool
  val top : 'a t -> 'a option
  val top_exn : 'a t -> 'a
  val top_with_key : 'a t -> (Key.t * 'a) option
  val top_with_key_exn : 'a t -> (Key.t * 'a)
  val pop_with_key : 'a t -> (Key.t * 'a) option
  val pop_with_key_exn : 'a t -> (Key.t * 'a)
  val pop : 'a t -> 'a option
  val pop_exn : 'a t -> 'a
  val cond_pop_with_key : 'a t -> (key:Key.t -> data:'a -> bool) -> (Key.t * 'a) option
  val cond_pop : 'a t -> ('a -> bool) -> 'a option
  val find : 'a t -> Key.t -> 'a option
  val find_pop : 'a t -> Key.t -> 'a option
  val find_exn : 'a t -> Key.t -> 'a
  val find_pop_exn : 'a t -> Key.t -> 'a
  val iter : 'a t -> f:(key:Key.t -> data:'a -> unit) -> unit
  val iter_vals : 'a t -> f:('a -> unit) -> unit
end

module Make (Key : Key) : S with module Key = Key = struct
  module Key = Key
  module Table = Hashtbl.Make (Key)

  type 'a t = {
    heap: (Key.t * 'a) Heap.t;
    tbl: (Key.t * 'a) Heap.heap_el Table.t;
  }

  let create ?min_size cmp =
    let initial_tbl_size =
      match min_size with
      | None -> 50
      | Some s -> s
    in
    { heap = Heap.create ?min_size (fun (_, v1) (_, v2) -> cmp v1 v2);
      tbl = Table.create ~size:initial_tbl_size ();
    }

  let copy t =
    {
      heap = Heap.copy t.heap;
      tbl = Hashtbl.copy t.tbl;
    }

  let push t ~key ~data =
    match Hashtbl.find t.tbl key with
    | Some _ -> `Key_already_present
    | None ->
        let el = Heap.push t.heap (key, data) in
        Hashtbl.replace t.tbl ~key ~data:el;
        `Ok

  let push_exn t ~key ~data =
    match push t ~key ~data with
    | `Ok -> ()
    
    | `Key_already_present -> failwith "Hash_heap: key already present"

  let replace t ~key ~data =
    match Hashtbl.find t.tbl key with
    | None -> push_exn t ~key ~data
    | Some el -> Heap.update el (key, data)

  let remove t key =
    match Hashtbl.find t.tbl key with
    | None -> ()
    | Some el ->
        Hashtbl.remove t.tbl key;
        Heap.remove el
  let mem t key = Hashtbl.mem t.tbl key

  let top_with_key t =
    match Heap.top t.heap with
    | None -> None
    | Some (k, v) -> Some (k, v)

  let top t =
    match top_with_key t with
    | None -> None
    | Some (_, v) -> Some v

  let top_exn t = snd (Heap.top_exn t.heap)

  let top_with_key_exn t = Heap.top_exn t.heap

  let pop_with_key_exn t =
    let (k, v) = Heap.pop_exn t.heap in
    Hashtbl.remove t.tbl k;
    (k, v)

  let pop_with_key t =
    try Some (pop_with_key_exn t)
    with Heap.Empty -> None

  let pop t =
    match pop_with_key t with
    | None -> None
    | Some (_, v) -> Some v

  let pop_exn t = snd (pop_with_key_exn t)

  let cond_pop_with_key t f =
    match Heap.cond_pop t.heap (fun (k, v) -> f ~key:k ~data:v) with
    | None -> None
    | Some (k, v) ->
        Hashtbl.remove t.tbl k;
        Some (k, v)

  let cond_pop t f =
    match cond_pop_with_key t (fun ~key:_ ~data -> f data) with
    | None -> None
    | Some (_k, v) -> Some v

  let find t key =
    match Hashtbl.find t.tbl key with
    | None -> None
    | Some el -> Some (snd (Heap.heap_el_get_el el))

  let find_exn t key =
    match find t key with
    | Some el -> el
    | None -> failwith "Hash_heap.find_exn"

  let find_pop t key =
    match Hashtbl.find t.tbl key with
    | None -> None
    | Some el ->
        let (_k, v) = Heap.heap_el_get_el el in
        Hashtbl.remove t.tbl key;
        Heap.remove el;
        Some v

  let find_pop_exn t key =
    match find_pop t key with
    | Some el -> el
    | None -> failwith "Hash_heap.find_pop_exn"

  let iter t ~f = Heap.iter t.heap ~f:(fun (k, v) -> f ~key:k ~data:v)
  let iter_vals t ~f = Heap.iter t.heap ~f:(fun (_k, v) -> f v)
end
