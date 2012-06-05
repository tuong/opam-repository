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

(**
 * Compliant simple CSV writter.
 *
 * This library is designed to deal with proper CSV (no quotes allowed in the
 * middle of the fields...). It is fast and flexible: by splitting most
 * writing functions in two parts one that gives the length of the string to
 * write and another that writes the result in a subpart of another string we
 * avoid unnecessary string creations.
 *)


(** Prints a valid csv file to a given channel (note that line are ended
    "\r\n") *)
val output_lines :
  ?quote:char
  -> ?sep:char
  -> out_channel
  -> string list list
  -> unit

(** Convert one CSV line to a string. *)
val line_to_string :
  ?quote:char
  -> ?sep:char
  -> string list
  -> string

(** Escape the a CSV field if need be.*)
val maybe_escape_field :
  ?quote:char
  -> ?sep:char
  -> string
  -> string

(** Escape a CSV (even if doesn't have any characters that require escaping).*)
val escape_field :
  ?quote:char
  -> string
  -> string

(** {3 Low-level } *)

(** Get the escaped length of one quoted field (without the quotes). Returns
    None if the field doesn't need to be escaped. *)
val quote_len:
  quote:char
  -> sep:char
  -> pos:int
  -> len:int
  -> string
  -> int option

(** Copy and escapes the content of a field over from one string to
    another. This does not put the quotes in.*)
val quote_blit:
  quote:char
  -> src:string
  -> dst:string
  -> src_pos:int
  -> dst_pos:int
  -> len :int
  -> int
