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

open Core.Std

(* a single 63 bit chunk of the array, bounds checking is left to the main module. We can
   only use 62 bits, because of the sign bit *)
module Int63_chunk : sig
  type t

  val empty : t
  val get : t -> int -> bool
  val set : t -> int -> bool -> t
end = struct
  open Int63

  type t = Int63.t

  let empty = zero

  let get t i = bit_and t (shift_left one i) > zero

  let set t i v =
    if v then bit_or t (shift_left one i)
    else bit_and t (bit_xor minus_one (shift_left one i))
end

type t = {
  data: Int63_chunk.t Array.t;
  length: int
}

type sexpable = t

(* We can't use the sign bit, so we only get to use 62 bits *)
let bits_per_bucket = 62

let create sz =
  if sz < 0 || sz > (Sys.max_array_length * bits_per_bucket) then
    invalid_argf "invalid size" ();
  { data = Array.create (1 + (sz / bits_per_bucket)) Int63_chunk.empty;
    length = sz }
;;

let bucket i = i / bits_per_bucket
let index i = i mod bits_per_bucket
let bounds_check t i =
  if i < 0 || i >= t.length then
    invalid_argf "Bitarray: out of bounds" ();
;;

let get t i =
  bounds_check t i;
  Int63_chunk.get t.data.(bucket i) (index i)
;;

let set t i v =
  bounds_check t i;
  let bucket = bucket i in
  t.data.(bucket) <- Int63_chunk.set t.data.(bucket) (index i) v
;;

let clear t =
  Array.fill t.data ~pos:0 ~len:(Array.length t.data) Int63_chunk.empty
;;

let fold =
  let rec loop t n ~init ~f =
    if n < t.length then
      loop t (n + 1) ~init:(f init (get t n)) ~f
    else
      init
  in
  fun t ~init ~f -> loop t 0 ~init ~f
;;

let iter t ~f = fold t ~init:() ~f:(fun _ v -> f v)

let sexp_of_t t =
  Array.sexp_of_t Bool.sexp_of_t
    (Array.init t.length ~f:(fun i -> get t i))
;;

let t_of_sexp sexp =
  let a = Array.t_of_sexp Bool.t_of_sexp sexp in
  let t = create (Array.length a) in
  Array.iteri a ~f:(fun i v -> set t i v);
  t
;;
