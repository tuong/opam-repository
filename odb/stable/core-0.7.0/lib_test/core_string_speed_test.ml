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

open Core.Std


(* let escape = String.Escaping.escape ~escapeworthy:['%';'^'] ~escape_char:'\\'
let escape_gen = String.Escaping.escape_gen ~escapeworthy_map:['%','%';'^','^'] ~escape_char:'\\'

let unescape = String.Escaping.unescape ~escape_char:'\\'
let unescape_gen = String.Escaping.unescape_gen ~map:[] ~escape_char:'\\'

let () =
  let now = Utime.now () in
  for i = 1 to 500_000 do
    ignore (escape_gen
    "foo%bar\\^baz^quux%%%%%%%%%%%%%%%aoeusnthoaeusntaohusoaeusnatohunsaoehusnaoehusnaotusnaoehuasoenuht^^^^^^^^^")
  done;
  let later = Utime.now () in
  prerr_endline (Utime.span_to_string (Utime.abs_diff later now));

  let now = Utime.now () in
  for i = 1 to 500_000 do
    ignore (escape
    "foo%bar\\^baz^quux%%%%%%%%%%%%%%%aoeusnthoaeusntaohusoaeusnatohunsaoehusnaoehusnaotusnaoehuasoenuht^^^^^^^^^")
  done;
  let later = Utime.now () in
  prerr_endline (Utime.span_to_string (Utime.abs_diff later now));



  let now = Utime.now () in
  for i = 1 to 500_000 do
    ignore (unescape_gen
    "foo%bar\\^baz^quux\\s\\s\\s\\s\\s\\s\\s\\s%%%%%%%%%%%%%%%aoeusnthoaeusntaohusoaeusnatohunsaoehusnaoehusnaotusnaoehuasoenuht^^^^^^^^^")
  done;
  let later = Utime.now () in
  prerr_endline (Utime.span_to_string (Utime.abs_diff later now));

  let now = Utime.now () in
  for i = 1 to 500_000 do
    ignore (unescape
    "foo%bar\\^baz^quux\\s\\s\\s\\s\\s\\s\\s\\s%%%%%%%%%%%%%%%aoeusnthoaeusntaohusoaeusnatohunsaoehusnaoehusnaotusnaoehuasoenuht^^^^^^^^^")
  done;
  let later = Utime.now () in
  prerr_endline (Utime.span_to_string (Utime.abs_diff later now)); *)
