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

exception Timeout

type 'a forked_computation = ('a,Sexp.t) Result.t with sexp

let run_in_fork ~f ~sexp_of v =
  let pipe_read,pipe_write = Unix.pipe () in
  match Unix.fork() with
  | 0 ->
      Unix.close pipe_read;
      let oc = Unix.out_channel_of_descr pipe_write in
      let res =
        try
          Ok (f v)
        with e -> Error (Exn.sexp_of_t e)
      in
      Sexp.output oc (sexp_of_forked_computation sexp_of res);
      exit 0
  | pid ->
      Unix.close pipe_write;
      pid,pipe_read

(** All our input comes in one burst so we do not need to run a select loop... *)
let wait_for_input ~timeout fd =
  let select_fds =
    Unix.select ()
      ~restart:true
      ~timeout
      ~read:[fd]
      ~write:[]
      ~except:[]
  in
  if select_fds.Unix.Select_fds.read = [] then
    None
  else
    Some (In_channel.input_all (Unix.in_channel_of_descr fd))

let waitpid_non_intr pid =
  let rec loop num_tries_remaining =
    if num_tries_remaining = 0 then
      failwith "Process.available_fds call to select failed too many times";
    try Unix.waitpid ~mode:[] pid
    with Unix.Unix_error (Unix.EINTR, _, _) -> loop (pred num_tries_remaining)
  in
  loop 1_000

let run ~timeout ~f ~sexp_of ~of_sexp v =
  let pid,pipe_read = run_in_fork ~f ~sexp_of v in
  protectx ()
    ~f:(fun () ->
          match wait_for_input ~timeout pipe_read with
          | None ->
              (* We timed out *)
              Process.kill ~is_child:true pid;
              raise Timeout
          | Some s ->
              let status = snd (waitpid_non_intr pid) in
              if status <> `Exited 0 then begin
                failwithf "Timed forked-out process exited with status %s"
                  (Unix.Process_status.to_string_hum status)
                  ()
              end;
              match forked_computation_of_sexp of_sexp (Sexp.of_string s) with
              | Result.Error e ->
                  failwithf "Timed forked-out function died with exception %s"
                    (Sexp.to_string_hum e)
                    ();
              | Result.Ok ok -> ok)
    ~finally:(fun () -> Unix.close pipe_read)
