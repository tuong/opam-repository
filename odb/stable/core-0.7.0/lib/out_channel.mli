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



type t = out_channel

val stdout : t
val stderr : t


val create : ?binary:bool -> ?append:bool -> ?perm:int -> string -> t

val close : t -> unit
val close_noerr : t -> unit

val set_binary_mode : t -> bool -> unit

val flush : t -> unit


val output : t -> buf:string -> pos:int -> len:int -> unit
val output_string : t -> string -> unit
val output_char : t -> char -> unit
val output_byte : t -> int -> unit
val output_binary_int : t -> int -> unit
val output_value : t -> _ -> unit

val newline : t -> unit
val output_lines : t -> string list -> unit

val seek : t -> int64 -> unit
val pos : t -> int64
val length : t -> int64
