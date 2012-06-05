(******************************************************************************
 *                             Bin-prot                                       *
 *                                                                            *
 * Copyright (C) 2008- Jane Street Holding, LLC                               *
 *    Contact: opensource@janestreet.com                                      *
 *    WWW: http://www.janestreet.com/ocaml                                    *
 *    Author: Markus Mottl                                                    *
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

(* Unsafe_common: functions common to unsafe binary protocol conversion. *)

open Common
open Bigarray

type sptr
type eptr
type sptr_ptr

external get_sptr : buf -> pos : pos -> sptr = "get_buf_ptr_stub" "noalloc"
external get_eptr : buf -> pos : pos -> eptr = "get_buf_ptr_stub" "noalloc"
external shift_sptr : sptr -> int -> sptr = "shift_sptr_stub" "noalloc"

external get_eptr_from_sptr_ptr :
  sptr_ptr -> pos : pos -> eptr = "get_eptr_from_sptr_ptr" "noalloc"

external get_buf_pos :
  start : sptr -> cur : sptr -> pos = "get_buf_pos_stub" "noalloc"

external get_safe_buf_pos :
  buf -> start : sptr -> cur : sptr -> pos = "get_safe_buf_pos_stub" "noalloc"

external alloc_sptr_ptr :
  buf -> pos : pos -> sptr_ptr = "alloc_sptr_ptr_stub" "noalloc"

external dealloc_sptr_ptr :
  buf -> sptr_ptr -> pos = "dealloc_sptr_ptr_stub" "noalloc"

external get_sptr_ptr : sptr_ptr -> buf -> pos = "get_sptr_ptr_stub" "noalloc"

external set_sptr_ptr :
  sptr_ptr -> buf -> pos : pos -> unit = "set_sptr_ptr_stub" "noalloc"

external get_sptr_ptr_sptr :
  sptr_ptr -> sptr = "get_sptr_ptr_sptr_stub" "noalloc"

external set_sptr_ptr_sptr :
  sptr_ptr -> sptr -> unit = "set_sptr_ptr_sptr_stub" "noalloc"

external get_ptr_string : sptr -> eptr -> string = "get_ptr_string_stub"

let get_read_init buf ~pos_ref =
  let start_pos = !pos_ref in
  if start_pos < 0 then array_bound_error ()
  else
    let buf_len = Array1.dim buf in
    if start_pos > buf_len then raise Buffer_short
    else
      let sptr_ptr = alloc_sptr_ptr buf ~pos:start_pos in
      let eptr = get_eptr buf ~pos:buf_len in
      sptr_ptr, eptr
