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

open StdLabels
open MoreLabels
open Sexplib.Conv
open Bin_prot
open Bin_prot.Std

let seek_out = `Deprecated_use_out_channel
let pos_out = `Deprecated_use_out_channel
let out_channel_length = `Deprecated_use_out_channel
let seek_in = `Deprecated_use_in_channel
let pos_in = `Deprecated_use_in_channel
let in_channel_length = `Deprecated_use_in_channel
let modf = `Deprecated_use_float_modf
let truncate = `Deprecated_use_float_round_towards_zero

let close_in = In_channel.close
let close_out = Out_channel.close

(** handy types for marking things read-only and read-write *)
type read_only with bin_io, sexp
type immutable  = private read_only with bin_io, sexp
type read_write = private read_only with bin_io, sexp
type write_only with bin_io, sexp

let sexp_of_immutable _ = failwith "attempt to convert abstract type immutable"
let immutable_of_sexp = sexp_of_immutable
let sexp_of_read_only _ = failwith "attempt to convert abstract type read_only"
let read_only_of_sexp = sexp_of_read_only
let sexp_of_read_write _ = failwith "attempt to convert abstract type read_write"
let read_write_of_sexp = sexp_of_read_write
let sexp_of_write_only _ = failwith "attempt to convert abstract type write_only"
let write_only_of_sexp = sexp_of_write_only

type never_returns
let never_returns (_ : never_returns) = assert false

exception Finally = Exn.Finally

let protectx = Exn.protectx
let protect = Exn.protect

let critical_section = Mutex0.critical_section

let (|!) = Fn.(|!)
let ident = Fn.id
let const = Fn.const

(* Begin code (mostly) copied from pervasives.ml *)

(* core_sys_open is the same as caml_sys_open, except it does not re-acquire the runtime
   lock until after its call to fcntl.
   this code should be removed when this fix makes it into the caml runtime
*)
external open_desc: string -> open_flag list -> int -> int = "core_sys_open"

external open_descriptor_out: int -> out_channel = "caml_ml_open_descriptor_out"
external open_descriptor_in: int -> in_channel = "caml_ml_open_descriptor_in"

(* General input functions *)

let open_in_gen mode perm name =
  open_descriptor_in (open_desc name mode perm)

let open_in name =
  open_in_gen [Open_rdonly; Open_text] 0 name

let open_in_bin name =
  open_in_gen [Open_rdonly; Open_binary] 0 name

let open_out_gen mode perm name =
  open_descriptor_out (open_desc name mode perm)

let open_out name =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_text] 0o666 name

let open_out_bin name =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o666 name

(* End code (mostly) copied from pervasives.ml *)

let read_wrap ?(binary = true) ~f fname =
  let ic =
    if binary then
      open_in_bin fname
    else
      open_in fname
  in
  protectx ic ~f ~finally:In_channel.close

let write_wrap ?(binary = true) ~f fname =
  let oc =
    if binary then
      open_out_bin fname
    else
      open_out fname
  in
  protectx oc ~f ~finally:Out_channel.close

let write_lines fname lines =
  write_wrap fname ~f:(fun oc -> Out_channel.output_lines oc lines)

let input_lines = In_channel.input_lines

let read_lines fname = read_wrap fname ~f:input_lines

let uw = function Some x -> x | None -> raise Not_found

let is_none = Option.is_none
let is_some = Option.is_some

let fst3 (x,_,_) = x
let snd3 (_,y,_) = y
let trd3 (_,_,z) = z

external ascending : 'a -> 'a -> int = "%compare"
let descending x y = compare y x

open Sexplib

let failwithf = Core_printf.failwithf
let invalid_argf = Core_printf.invalid_argf
let exitf = Core_printf.exitf

(* module With_return only exists to avoid circular dependencies *)
include With_return

let equal = Caml.(=)

let phys_equal = Caml.(==)
let (==) _ _ = `Consider_using_phys_equal
let (!=) _ _ = `Consider_using_phys_equal

let force = Lazy.force

let ( ^/ ) = Core_filename.concat


type decimal = float with bin_io
let sexp_of_decimal x = Sexp.Atom (Core_printf.sprintf "%.12G" x)
let decimal_of_sexp = function
  | Sexp.Atom s ->
    let result = Float.of_string s in
    begin match Pervasives.classify_float result with
    | FP_normal
    | FP_subnormal
    | FP_zero ->
      result
    | FP_infinite
    | FP_nan ->
      Conv.of_sexp_error "decimal_of_sexp: nan or inf" (Sexp.Atom s)
    end
  | s ->
    Conv.of_sexp_error "decimal_of_sexp: Expected Atom, found List" s

type 'a bound = Incl of 'a | Excl of 'a | Unbounded
type passfail = Pass | Fail of string

exception Validation_error of string list with sexp
exception Unimplemented of string with sexp
exception Bug of string with sexp
