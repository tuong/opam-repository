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

(* Test Condition.timedwait *)

open Core.Std
open Core_extended.Std

open OUnit

let run () =
  let mtx = Mutex.create () in
  let cnd = Condition.create () in
  let condition = ref false in
  let timedout = ref false in
  let condition_setter () =
    Thread.delay 0.2;
    Mutex.lock mtx;
    condition := true;
    Condition.signal cnd;
    Mutex.unlock mtx;
  in
  let _tid = Thread.create condition_setter () in
  Mutex.lock mtx;
  let rec loop () =
    let timeout = Unix.gettimeofday () +. 0.15 in
    if Condition.timedwait cnd mtx (Time.of_float timeout) then (
      assert !timedout;
      assert !condition)
    else (
      assert (not !timedout);
      timedout := true;
      loop ())
  in
  loop ();
  printf "Condition.timedwait succeeded\n\n%!"

let test = "Condition_test" >::: [
  "test" >:: (fun () ->
    "1" @? (try run (); true with e ->
              eprintf "in condition test:%s\n%!" (Exn.to_string e);
              false));
]
