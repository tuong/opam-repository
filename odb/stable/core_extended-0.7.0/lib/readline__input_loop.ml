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

module Term = Console.Ansi
module LZ = Readline__list_zipper
module SZ = Readline__string_zipper

type t = {
  hist : string LZ.t;
  line : SZ.t
}

let create ?(text="") hist =
  let hist = LZ.create [] hist in
  let line = SZ.create text "" in
  { hist = hist; line = line}

let contents v = SZ.contents v.line

let print ~prompt ~map_out v =
  Term.home_cursor ();
  Term.kill_line ();
  print_string prompt;
  print_string (map_out (SZ.left_contents v.line));
  Term.save_cursor();
  print_string (map_out (SZ.right_contents v.line));
  Term.unsave_cursor();
  flush stdout

type input = [ `Backspace
   | `Char of char
   | `Delete
   | `Down
   | `End
   | `Eof
   | `Home
   | `Left
   | `Right
   | `Tab
   | `Unknown_escape of (string*int option*int option)
   | `Up ]

let complete ~f v =
  let leftp = SZ.left_contents v.line in
  let rightp = SZ.right_contents v.line in
  match f ~left:leftp ~right:rightp with
  | [left] ->
    {v with line = SZ.create left ""}
  | [] ->
      Term.bell();
      v
  | matches ->
      (* TODO: Multiple entries on one line with console.*)
      print_newline ();
      List.iter matches ~f:print_endline;
      v

let backspace v =
  match SZ.drop_before v.line with
  | None -> v
  | Some (_,l) -> {v with line = l}

let delete v =
  match SZ.drop_after v.line with
  | None -> v
  | Some (_,l) -> {v with line = l }

let up_history v =
  match LZ.drop_after v.hist with
  | None -> v
  | Some (e,h) ->
      { hist = LZ.insert_before h (SZ.contents v.line);
        line = SZ.create e "" }

let down_history v =
  match LZ.drop_before v.hist with
  | None -> v
  | Some (e,h) ->
      {hist = LZ.insert_after h (SZ.contents v.line);
       line = (SZ.create e "") }

let cursor_left v =
  { v with line = Option.value (SZ.previous v.line) ~default:v.line }

let cursor_right v =
  { v with line = Option.value (SZ.next v.line) ~default:v.line }

let step ?completion (v:t) : input -> t = function
  | `Tab ->
      begin match completion with
      | Some f -> complete ~f v
      | None   ->
          { v with line = SZ.insert_before v.line '\t' }
      end
  | `Backspace        -> backspace v
  | `Delete           -> delete v
  | `Up               -> up_history v
  | `Down             -> down_history v
  | `Left             -> cursor_left v
  | `Right            -> cursor_right v
  | `Home             -> { v with line = SZ.first v.line }
  | `End              -> { v with line = SZ.last v.line }
  | `Unknown_escape _ -> v
  | `Char c           -> { v with line = SZ.insert_before v.line c }
  | `Eof              -> raise End_of_file
