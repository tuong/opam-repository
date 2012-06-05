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

open Std_internal
module Unix = Core_unix

module Thread = Core_thread



let default_umask = 0

let check_threads () =
  (* forking, especially to daemonize, when running multiple threads is tricky, and
    generally a mistake.  It's so bad, and so hard to catch, that we test in two
    different ways *)
  if Thread.threads_have_been_created () then
    failwith
      "Daemon.check_threads: may not be called \
      if any threads have ever been created";
  begin match Thread.num_threads () with
  | None -> ()  (* This is pretty bad, but more likely to be a problem with num_threads *)
  | Some (1 | 2) -> () (* main thread, or main + ticker - both ok *)
  | Some _ ->
      failwith
        "Daemon.check_threads: may not be called if more than 2 threads \
        (hopefully the main thread + ticker thread) are running"
  end;
;;

let dup_null ~skip_regular_files ~mode ~dst =
  
  let is_regular () =
    try (Unix.fstat dst).Unix.st_kind = Unix.S_REG
    with Unix.Unix_error (Unix.EBADF, _, _) -> false
  in
  if not (skip_regular_files && is_regular ()) then begin
    let null = Unix.openfile "/dev/null" ~mode:[mode] ~perm:0o777 in
    Unix.dup2 ~src:null ~dst;
    Unix.close null;
  end;
;;

let close_stdio_fds ~skip_regular_files =
  
  dup_null ~skip_regular_files ~mode:Unix.O_RDONLY ~dst:Unix.stdin;
  dup_null ~skip_regular_files ~mode:Unix.O_WRONLY ~dst:Unix.stdout;
  dup_null ~skip_regular_files ~mode:Unix.O_WRONLY ~dst:Unix.stderr;
;;


let daemonize ?(close_stdio=true) ?(cd = "/") ?umask:(umask_value = default_umask) () =
  check_threads ();
  let fork_no_parent () =
    let pid = Unix.handle_unix_error Unix.fork () in
    assert (pid >= 0);
    if pid <> 0 then exit 0
  in
  (* Fork into the background, parent exits, child continues. *)
  fork_no_parent ();
  (* Become session leader. *)
  ignore (Unix.Terminal_io.setsid ());
  (* Fork again to ensure that we will never regain a controlling terminal. *)
  fork_no_parent ();
  (* Release old working directory. *)
  Unix.chdir cd;
  (* Ensure sensible umask.  Adjust as needed. *)
  ignore (Unix.umask umask_value);
  if close_stdio then close_stdio_fds ~skip_regular_files:false;
;;

let fail_wstopped ~pid ~i =
  failwithf "Bug: waitpid on process %i returned WSTOPPED %i, \
    but waitpid not called with WUNTRACED.  This should not happen" i pid ()

let daemonize_wait ?(cd = "/") ?umask:(umask_value = default_umask) () =
  check_threads ();
  let pid = Unix.handle_unix_error Unix.fork () in
  assert (pid >= 0);
  if pid = 0 then begin
    ignore (Unix.Terminal_io.setsid ());
    let read_end, write_end = Unix.pipe () in
    let buf = "done" in
    let len = String.length buf in
    let pid = Unix.handle_unix_error Unix.fork () in
    if pid = 0 then begin
      (* The process that will become the actual daemon. *)
      Unix.close read_end;
      Unix.chdir cd;
      ignore (Unix.umask umask_value);
      (fun () ->
        close_stdio_fds ~skip_regular_files:true;
        let old_sigpipe_behavior = Signal.signal Signal.pipe `Ignore in
        (try ignore (Unix.write write_end ~buf ~pos:0 ~len : int) with _ -> ());
        Signal.set Signal.pipe old_sigpipe_behavior;
        Unix.close write_end
      )
    end else begin
      (* The middle process, after it has forked its child. *)
      Unix.close write_end;
      let rec loop () =
        let wait_result, process_status =
          Caml.UnixLabels.waitpid ~mode:[Caml.UnixLabels.WNOHANG] pid in
        if wait_result = 0 then begin
          match Caml.Unix.select [read_end] [] [] 0.1 with
          | [read_end], [], [] ->
              ignore ((Unix.read read_end ~buf:(String.create len) ~pos:0 ~len):int);
              exit 0
          | _, _, _ -> loop ()
        end else
          match process_status with
          | Caml.Unix.WEXITED i | Caml.Unix.WSIGNALED i -> exit i
          | Caml.Unix.WSTOPPED i -> fail_wstopped ~pid ~i
      in loop ()
    end
  end else
    
    match snd (Caml.UnixLabels.waitpid ~mode:[] pid) with
    | Caml.Unix.WEXITED i | Caml.Unix.WSIGNALED i -> exit i
    | Caml.Unix.WSTOPPED i -> fail_wstopped ~pid ~i
;;
