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
module String = Core_string

let threads_have_been_created = ref false

include Caml.Thread

let create f arg =
  threads_have_been_created := true;
  let f arg =
    try f arg
    with x ->
      Printf.eprintf "%s\n%!" (Exn.to_string x);
      raise x
  in
  create f arg
;;

let threads_have_been_created () = !threads_have_been_created

let wait_signal sigs = wait_signal (List.map ~f:Signal.to_caml_int sigs)

let sigmask cmd sigs =
  let cmd =
    match cmd with
    | `Set -> Unix.SIG_SETMASK
    | `Block -> Unix.SIG_BLOCK
    | `Unblock -> Unix.SIG_UNBLOCK
  in
  let sigs = List.map ~f:Signal.to_caml_int sigs in
  List.map ~f:Signal.of_caml_int (sigmask cmd sigs)
;;

let num_threads () =
  let rec find_thread_count = function
    | [] -> None
    | line :: xs ->
        if String.is_prefix line ~prefix:"Threads:" then
          begin
            try
              Some (int_of_string
                       (String.strip (snd (String.lsplit2_exn line ~on:':'))))
            with
            | _ -> None
          end
        else find_thread_count xs
  in
  try
    find_thread_count
      (Common.read_lines
          ("/proc/" ^ string_of_int (Unix.getpid ()) ^ "/status"))
  with _ -> None
;;

let block_forever () =
  Event.sync (Event.receive (Event.new_channel ()))
;;
