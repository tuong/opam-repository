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
open Unix

external setresuid : ruid:int -> euid:int -> suid:int -> unit = "linux_setresuid_stub"

let setresuid ?(ruid= -1) ?(euid= -1) ?(suid= -1) () =
  setresuid ~ruid ~euid ~suid

type uids = {
  ruid:int;
  euid:int;
  suid:int
} with sexp,bin_io


external getresuid : unit -> uids = "linux_getresuid_stub"


(* Splicing - zero-copies between kernel buffers *)



module Splice = struct
  type flag = MOVE | NONBLOCK | MORE | GIFT with sexp, bin_io
  type flags

  external make_flags : flag array -> flags = "linux_splice_make_flags_stub"

  external unsafe_splice :
    bool ->
    fd_in : file_descr -> off_in : int ->
    fd_out : file_descr -> off_out : int ->
    len : int ->
    flags
    -> int * int * int = "linux_splice_stub_bc" "linux_splice_stub"

  let splice
        ?(assume_fd_is_nonblocking = false)
        ~fd_in ?off_in
        ~fd_out ?off_out ~len flags =
    let off_in =
      match off_in with
      | None -> -1
      | Some off_in when off_in < 0 -> invalid_arg "Splice.splice: off_in < 0"
      | Some off_in -> off_in
    in
    let off_out =
      match off_out with
      | None -> -1
      | Some off_out when off_out < 0 ->
          invalid_arg "Splice.splice: off_out < 0"
      | Some off_out -> off_out
    in
    if len < 0 then invalid_arg "Splice.splice: len < 0";
    unsafe_splice assume_fd_is_nonblocking ~fd_in ~off_in ~fd_out ~off_out ~len flags

  external unsafe_tee :
    bool -> fd_in : file_descr -> fd_out : file_descr -> int -> flags -> int
    = "linux_tee_stub"

  let tee ?(assume_fd_is_nonblocking = false) ~fd_in ~fd_out len flags =
    if len < 0 then invalid_arg "Splice.splice: len < 0";
    unsafe_tee assume_fd_is_nonblocking ~fd_in ~fd_out len flags

  external unsafe_vmsplice :
    bool -> file_descr -> int -> flags -> int = "linux_vmsplice_stub"

  let vmsplice ?(assume_fd_is_nonblocking = false) fd iovecs ?count flags =
    let count =
      match count with
      | None -> Array.length iovecs
      | Some count ->
          if count < 0 then invalid_arg "Splice.vmsplice: count < 0";
          let n_iovecs = Array.length iovecs in
          if count > n_iovecs then
            invalid_arg "Splice.vmsplice: count > n_iovecs";
          count
    in
    unsafe_vmsplice assume_fd_is_nonblocking fd count flags
end
