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
open OUnit

let try_with_mutex f =
  let mtx = Mutex.create () in
  try f mtx; false
  with Sys_error _ -> true

let check_double_lock () =
  try_with_mutex (fun mtx -> Mutex.lock mtx; Mutex.lock mtx)

let check_unlock_unlocked () = try_with_mutex Mutex.unlock

let check_unlock_other_locked () =
  try_with_mutex (fun mtx ->
    Thread.join (Thread.create (fun () -> Mutex.lock mtx) ());
    Mutex.unlock mtx)

let test =
  "core_mutex" >:::
    [
      "error checking" >:: (fun () ->
        "double lock" @? check_double_lock ();
        "unlock unlocked" @? check_unlock_unlocked ();
        "unlock other locked" @? check_unlock_other_locked ();
      )
    ]
