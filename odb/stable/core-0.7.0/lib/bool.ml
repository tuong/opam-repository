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

let invalid_argf = Core_printf.invalid_argf

module T = struct
  type t = bool with bin_io, sexp

  type binable = t
  type comparable = t
  type sexpable = t
  type stringable = t

  let compare (t : t) t' = compare t t'
  (* we use physical equality here because for bools it is the same *)
  let equal (t : t) t' = t == t'
  let hash x = if x then 1 else 0
end

include T

let of_string = function
  | "true" -> true
  | "false" -> false
  | s -> invalid_argf "Bool.of_string: expected true or false but got %s" s ()
;;

let to_string = string_of_bool


let min (x : t) y = if x < y then x else y
let max (x : t) y = if x > y then x else y
let ascending = compare
let descending x y = compare y x
let ( >= ) (x : t) y = x >= y
let ( <= ) (x : t) y = x <= y
let ( = ) = equal
let ( > ) (x : t) y = x > y
let ( < ) (x : t) y = x < y
let ( <> ) (x : t) y = x != y

(* Making bool hashable may seem frivolous, but consider an aggregate type with
   a bool in it that needs a custom hash function. *)
include Hashable.Make (T)
module Set = Core_set.Make (T)
module Map = Core_map.Make (T)
