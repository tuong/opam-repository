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

type ('perm, 'state) t = {
  mutable state : 'state option;
  updates : ('state -> 'state) Queue.t;
  mutable watchers : ('state -> unit) list;
  executing : Mutex.t;
}

let create ?init () = {
  state = init;
  updates = Queue.create ();
  watchers = [];
  executing = Mutex.create ();
}

let try_lock mtx =
  try Mutex.try_lock mtx with _ -> false

let clear_queue t =
  if try_lock t.executing
  then begin
    let rec loop () =
      match t.state with
      | None -> ()
      | Some state ->
        match Queue.dequeue t.updates with
        | None -> ()
        | Some f ->
          let new_state = f state in
          List.iter t.watchers ~f:(fun f -> f new_state);
          t.state <- Some new_state;
          loop ()
    in
    Exn.protect ~f:loop ~finally:(fun () -> Mutex.unlock t.executing; Queue.clear t.updates)
  end

let init t state =
  if Option.is_some t.state
  then failwith "Update_queue.init: Cannot call init twice"
  else begin
    t.state <- Some state;
    clear_queue t
  end

let enqueue t f =
  Queue.enqueue t.updates f;
  clear_queue t

let watch t ~f = t.watchers <- f :: t.watchers

let map t ~f =
  let new_t = create ?init:(Option.map t.state ~f) () in
  watch t ~f:(fun x -> enqueue new_t (fun _ -> f x));
  new_t


let read_only t =
  let new_t = create ?init:t.state () in
  watch t ~f:(fun x -> enqueue new_t (fun _ -> x));
  new_t
