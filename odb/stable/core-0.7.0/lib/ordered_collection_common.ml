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

let invalid_argf = Core_printf.invalid_argf

let normalize ~length_fun t i =
  if i < 0 then length_fun t + i else i


let slice ~length_fun ~sub_fun t start stop =
  let stop = if stop = 0 then length_fun t else stop in
  let pos = normalize ~length_fun t start in
  let len = (normalize ~length_fun t stop) - pos in
  sub_fun t ~pos ~len

let get_pos_len_exn ?(pos=0) ?len ~length =
  if pos < 0 then
    invalid_argf "Negative position: %d < 0" pos ();
  if pos > length then
    invalid_argf "Start position after the end: %d > %d"
      pos length ();
  let maxlen = length - pos in
  let len =
    match len with
    | None -> maxlen
    | Some len ->
        if len < 0 then
          invalid_argf "Negative length: %d" len ()
        else if len > maxlen then
          invalid_argf "pos + len past end: %d + %d > %d"
            pos len length ()
        else
          len
  in
  (pos, len)
;;

let get_pos_len ?pos ?len ~length =
  try Result.Ok (get_pos_len_exn ?pos ?len ~length)
  with Invalid_argument s -> Result.Error s

