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

open Sexplib.Wrapper
open Bin_prot.Std

type 'a t = 'a option with sexp

type 'a container = 'a t
type 'a sexpable = 'a t

let is_none = function None -> true | _ -> false

let is_some = function Some _ -> true | _ -> false

let value_map o ~default ~f =
  match o with
  | Some x -> f x
  | None   -> default

let iter o ~f =
  match o with
  | None -> ()
  | Some a -> f a

let map2 o1 o2 ~f =
  match o1, o2 with
  | Some a1, Some a2 -> Some (f a1 a2)
  | _ -> None

let call x ~f =
  match f with
  | None -> ()
  | Some f -> f x

let apply x ~f =
  match f with
  | None -> None
  | Some f -> Some (f x)

let value t ~default =
  match t with
  | None -> default
  | Some x -> x
;;

let value_exn t =
  match t with
  | Some x -> x
  | None -> failwith "Option.value_exn None"
;;

let value_exn_message message t =
  match t with
  | Some x -> x
  | None -> failwith message
;;

let to_array t =
  match t with
  | None -> [||]
  | Some x -> [|x|]
;;

let to_list t =
  match t with
  | None -> []
  | Some x -> [x]
;;

let for_all t ~f =
  match t with
  | None -> true
  | Some x -> f x
;;

let exists t ~f =
  match t with
  | None -> false
  | Some x -> f x
;;

let length t =
  match t with
  | None -> 0
  | Some _ -> 1
;;

let is_empty = is_none

let fold t ~init ~f =
  match t with
  | None -> init
  | Some x -> f init x
;;

let find t ~f =
  match t with
  | None -> None
  | Some x -> if f x then Some x else None
;;

let equal f t t' =
  match t, t' with
  | None, None -> true
  | Some x, Some x' -> f x x'
  | _ -> false

let some x = Some x

let both x y =
  match x,y with
  | Some a, Some b -> Some (a,b)
  | _ -> None

let filter ~f = function
  | Some v as o when f v -> o
  | _ -> None

let compare ~cmp v1 v2 =
  let tag_to_int = function Some _ -> 1 | None -> 0 in
  match v1,v2 with
  | Some v1,Some v2 -> cmp v1 v2
  | None,None -> 0
  | _ -> compare (tag_to_int v1) (tag_to_int v2)

let try_with f =
  try Some (f ())
  with _ -> None

include Monad.Make (struct
  type 'a t = 'a option
  let return x = Some x
  let bind o f =
    match o with
    | None -> None
    | Some x -> f x
  let failwith e = failwith e
end)

let container = {
  Container.
  length = length;
  is_empty = is_empty;
  iter = iter;
  fold = fold;
  exists = exists;
  for_all = for_all;
  find = find;
  to_list = to_list;
  to_array = to_array;
}
