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

type 'a t =
  {
    mutable v_opt : 'a option;
    mtx : Mutex.t;
    cond : Condition.t
  }

let init v_opt =
  {
    v_opt = v_opt;
    mtx = Mutex.create ();
    cond = Condition.create ()
  }

let signal sem v =
  Mutex.lock sem.mtx;
  let v_opt = sem.v_opt in
  sem.v_opt <- Some v;
  if v_opt = None then Condition.signal sem.cond;
  Mutex.unlock sem.mtx

let wait_return sem v =
  sem.v_opt <- None;
  Mutex.unlock sem.mtx;
  v

let rec wait_loop sem =
  Condition.wait sem.cond sem.mtx;
  match sem.v_opt with
  | None -> wait_loop sem
  | Some v -> wait_return sem v

let wait sem =
  Mutex.lock sem.mtx;
  match sem.v_opt with
  | None -> wait_loop sem
  | Some v -> wait_return sem v

let get sem =
  match sem.v_opt with
  | None as none -> none
  | _ ->
      Mutex.lock sem.mtx;
      let res = sem.v_opt in
      sem.v_opt <- None;
      Mutex.unlock sem.mtx;
      res

let look sem = sem.v_opt
