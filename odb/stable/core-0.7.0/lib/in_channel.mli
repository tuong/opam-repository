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

(** In_channel collects all of the pervasive functions that work on in_channels.
    * It adds some new functions (like [input_all] and [input_lines]).
    * It names things using the fact that there is no worry about toplevel name
      conflicts (since we are in a module).
    * It uses labelled arguments.
    * It returns an option rather than raising End_of_file.
*)

type t = in_channel

val stdin : t

val create : ?binary:bool -> string -> t

val close : t -> unit
val close_noerr : t -> unit             

val input : t -> buf:string -> pos:int -> len:int -> int
val really_input : t -> buf:string -> pos:int -> len:int -> unit option
val input_byte : t -> int option
val input_char : t -> char option

val input_binary_int : t -> int option

val input_value : t -> _ option
val input_all : t -> string


(** [input_line ?fix_win_eol t] reads a line from [t] and returns it, without
    the newline ("\n") character at the end, and, if [fix_win_eol] the trailing
    "\r\n" is dropped.
*)
val input_line : ?fix_win_eol:bool -> t -> string option

(** [fold_lines ?fix_win_eol t ~init ~f] folds over the lines read from [t]
    using [input_line].  Lines are provided to [f] in the order they are
    found in the file. *)
val fold_lines :
  ?fix_win_eol:bool -> t -> init:'a -> f:('a -> string -> 'a) -> 'a

(** [input_lines ?fix_win_eol t] returns the list of lines read from [t] using
    [input_line].
*)
val input_lines : ?fix_win_eol:bool -> t -> string list

(** [iter_lines ?fix_win_eol t ~f] applies [f] to each line read from [t] using
    [input_line]. *)
val iter_lines : ?fix_win_eol:bool -> t -> f:(string -> unit) -> unit

val seek : t -> int64 -> unit
val pos : t -> int64
val length : t -> int64

val set_binary_mode : t -> bool -> unit
