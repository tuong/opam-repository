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

module Big_int = Sexplib.Wrapper.Big_int
module Nat     = Sexplib.Wrapper.Nat
module Ratio   = Sexplib.Wrapper.Ratio
module Num     = Sexplib.Wrapper.Num
module Set = Core_set
module Map = Core_map
module Array = Core_array
include Array.Infix
module Hashtbl = Core_hashtbl
module String = Core_string
module List = Core_list
include List.Infix

module Queue = Core_queue
module Stack = Core_stack
module Sys = Core_sys
module Char = Core_char

module Bool = Bool
module Int = Core_int
include Int.Infix
module Int32 = Core_int32
module Int64 = Core_int64
module Nativeint = Core_nativeint

(* handy shortcuts *)
include Common

include (Float : Interfaces.Robustly_comparable with type robustly_comparable = float)
include String.Infix
let int_of_float = Float.to_int

(* Float.ceil and floor are excluded because we haven't changed them from the default *)
let round_towards_zero = Float.round_towards_zero
let round_towards_zero_exn = Float.round_towards_zero_exn
let round = Float.round
include Interfaces
module Sexp = Core_sexp
include Core_sexp.Sexp_option
include Core_sexp.Sexp_list
include Core_sexp.Sexp_array
include Core_sexp.Sexp_opaque
include Sexplib.Conv
include Printf
include Scanf
include Bin_prot.Std

include Result.Export

let sexp_of_array = Array.sexp_of_t
let array_of_sexp = Array.t_of_sexp
let sexp_of_bool = Bool.sexp_of_t
let bool_of_sexp = Bool.t_of_sexp

let sexp_of_char = Char.sexp_of_t
let char_of_sexp = Char.t_of_sexp
let sexp_of_exn = Exn.sexp_of_t
let sexp_of_float = Float.sexp_of_t
let float_of_sexp = Float.t_of_sexp
let sexp_of_int = Int.sexp_of_t
let int_of_sexp = Int.t_of_sexp
let sexp_of_int32 = Int32.sexp_of_t
let int32_of_sexp = Int32.t_of_sexp
let sexp_of_int64 = Int64.sexp_of_t
let int64_of_sexp = Int64.t_of_sexp
let sexp_of_list = List.sexp_of_t
let list_of_sexp = List.t_of_sexp
let sexp_of_nativeint = Nativeint.sexp_of_t
let nativeint_of_sexp = Nativeint.t_of_sexp
let sexp_of_option = Option.sexp_of_t
let option_of_sexp = Option.t_of_sexp
let sexp_of_string = String.sexp_of_t
let string_of_sexp = String.t_of_sexp
