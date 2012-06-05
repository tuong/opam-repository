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

open Std_internal

(** The type used to store function results for memoization. *)
type 'a memo_store  = Rval of 'a | Expt of exn

let do_memo_store = function
  | Rval rval -> rval
  | Expt e -> raise e

let capture_memo f x =
  try Rval (f x) with
  | Sys.Break as e -> raise e
  | e -> Expt e

(* Note that using (=) instead of compare would be a minor bug -- nan <> nan. *)
let ident ?(equal=(fun x y -> compare x y = 0)) f =
  let store = ref None in
  (fun arg ->
    let memo_store = match !store with
      | Some (oldarg, old_memo_store) when equal oldarg arg ->
          old_memo_store
      | _ ->
          let memo_store = capture_memo f arg in
          store := Some (arg, memo_store);
          memo_store
    in
    do_memo_store memo_store
  )

let unit f =
  let l = Lazy.lazy_from_fun f in
  (fun () -> Lazy.force l)


let general f =
  let store = Hashtbl.Poly.create () ~size:0 in
  (fun x ->
    let memo_store = Hashtbl.find_or_add store x
      ~default:(fun () -> capture_memo f x)
    in
    do_memo_store memo_store
  )
