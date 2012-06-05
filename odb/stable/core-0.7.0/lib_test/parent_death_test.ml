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

(* Test parent death notification *)

open Core.Std
open Sys
open Linux_ext
open OUnit

let handle_signal s =
  if Signal.equal s Signal.hup then
    ()
  else (
    printf "Got unknown signal: %s\n%!" (Signal.to_string s);
    assert false
  )

let run () =
  Signal.handle Signal.hup handle_signal;
  let pid = Unix.fork () in
  if pid = 0 then (
    pr_set_pdeathsig Signal.hup;
    if Unix.getppid () = 1 then ignore (Signal.send Signal.kill ~pid:(Unix.getpid ()));
    Unix.sleep 3)
  else Unix.sleep 1

let test = "Parent_death_test" >::: [
  "test" >:: (fun () ->
    "1" @? (try run (); true with _ -> false));
]
