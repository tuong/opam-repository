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
  (** Printing on console tty's  *)

(** Handling of ansi codes. *)
module Ansi : sig
  val kill_line : unit -> unit
  val bell : unit -> unit
  val home_cursor : unit -> unit
  val save_cursor : unit -> unit
  val unsave_cursor : unit -> unit

  type color = [
  | `Black
  | `Red
  | `Green
  | `Yellow
  | `Blue
  | `Magenta
  | `Cyan
  | `White
  ]

  type attr = [
  | `Bright
  | `Dim
  | `Underscore
  | `Reverse
  | color
  | `Bg of color
  ]

  val printf  : attr list -> ('a, out_channel, unit) format -> 'a
  val eprintf : attr list -> ('a, out_channel, unit) format -> 'a

  val output_string : attr list -> out_channel -> string -> unit
  val output : attr list -> out_channel -> string -> int -> int -> unit
end

val is_color_tty : unit -> bool

(** The width in characters of the current output. Returns [None] if stdout is
    not connected to a tty.*)
val width : unit -> int option

(** print a list in a columnize way (like the output of ls) *)
val print_list : out_channel -> (string * Ansi.attr list) list -> unit
