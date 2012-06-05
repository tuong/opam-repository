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

(** unpack a signed 1 byte int from the given buf starting at pos *)
val unpack_signed_8 : buf:string -> pos:int -> int

(** unpack an unsigned 1 byte int from the given buf starting at pos *)
val unpack_unsigned_8 : buf:string -> pos:int -> int

(** encode and pack the specified int as a signed 1 byte int, and place it in the buffer
    starting at pos *)
val pack_signed_8 : buf:string -> pos:int -> int -> unit

(** encode and pack the specified int as an unsigned 1 byte int, and place it in the
    buffer starting at pos *)
val pack_unsigned_8 : buf:string -> pos:int -> int -> unit

(** unpack a signed 2 byte int from the given buf starting at pos. By default the byte
    order is `Big_endian. *)
val unpack_signed_16 : byte_order:[`Big_endian | `Little_endian] -> buf:string -> pos:int -> int

(** encode and pack the specified int as a signed 2 byte int, and place it in the buffer
    starting at pos. By default the byte order is `Big_endian. *)
val pack_signed_16 : byte_order:[`Big_endian | `Little_endian] -> buf:string -> pos:int -> int -> unit

(** unpack a signed 4 byte int32 from the given buf starting at pos. By default the byte
    order is `Big_endian. *)
val unpack_signed_32 : byte_order:[`Big_endian | `Little_endian] -> buf:string -> pos:int -> int32

(** unpack a signed 4 byte int32 from the given buf starting at pos. DO NOT USE ON A 32
    BIT COMPUTER! By default the byte order is `Big_endian. *)
val unpack_signed_32_int : byte_order:[`Big_endian | `Little_endian] -> buf:string -> pos:int -> int

(** encode and pack the specified int32 as a signed 4 byte int, and place it in the buffer
    starting at pos. By default the byte order is `Big_endian. *)
val pack_signed_32 : byte_order:[`Big_endian | `Little_endian] -> buf:string -> pos:int -> Int32.t -> unit

(** encode and pack the specified int as a signed 4 byte int, and place it in the buffer
    starting at pos. DO NOT USE ON A 32 BIT COMPUTER!. By default the byte order is
    `Big_endian. *)
val pack_signed_32_int : byte_order:[`Big_endian | `Little_endian] -> buf:string -> pos:int -> int -> unit

(** encode and pack the specified int64 as a signed 8 byte int, and place it in the buffer
    starting at pos. By default the byte order is `Big_endian. *)
val pack_signed_64 : byte_order:[`Big_endian | `Little_endian] -> buf:string -> pos:int -> Int64.t -> unit

(** encode and pack the specified int as a signed 8 byte int, and place it in the buffer
    starting at pos. DO NOT USE ON A 32 BIT COMPUTER! By default the byte order is
    `Big_endian. *)
val pack_signed_64_int : byte_order:[`Big_endian | `Little_endian] -> buf:string -> pos:int -> int -> unit

(** unpack a signed 8 byte int64 from the given buf starting at pos. By default the byte
    order is `Big_endian. *)
val unpack_signed_64 : byte_order:[`Big_endian | `Little_endian] -> buf:string -> pos:int -> int64

(** unpack a signed 8 byte int from the given buf starting at pos. DO NOT USE ON A 32 BIT
    COMPUTER! By default the byte order is `Big_endian. *)
val unpack_signed_64_int : byte_order:[`Big_endian | `Little_endian] -> buf:string -> pos:int -> int

val unpack_float : byte_order:[`Big_endian | `Little_endian] -> buf:string -> pos:int -> float
val pack_float : byte_order:[`Big_endian | `Little_endian] -> buf:string -> pos:int -> float -> unit

val test : unit -> unit
