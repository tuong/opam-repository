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

module List = Core_list

type t = out_channel

let seek = Pervasives.LargeFile.seek_out
let pos = Pervasives.LargeFile.pos_out
let length = Pervasives.LargeFile.out_channel_length

let stdout = Pervasives.stdout
let stderr = Pervasives.stderr

let create ?(binary = false) ?(append = false) ?(perm = 0o644) file =
  let flags = [Open_wronly; Open_creat] in
  let flags = (if binary then Open_binary else Open_text) :: flags in
  let flags = (if append then Open_append else Open_trunc) :: flags in
  Pervasives.open_out_gen flags perm file
;;

let set_binary_mode = Pervasives.set_binary_mode_out

let flush = Pervasives.flush

external close_out_channel : t -> unit = "fixed_close_channel";;
let close t = flush t; close_out_channel t
let close_noerr t =
  (try flush t with _ -> ());
  (try close_out_channel t with _ -> ())

let output t ~buf ~pos ~len = Pervasives.output t buf pos len
let output_string = Pervasives.output_string
let output_char = Pervasives.output_char
let output_byte = Pervasives.output_byte
let output_binary_int = Pervasives.output_binary_int
let output_value = Pervasives.output_value

let newline t = output_string t "\n"

let output_lines t lines =
  List.iter lines ~f:(fun line ->
    output_string t line;
    newline t)
;;
