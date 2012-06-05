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

(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
open Core.Std


(**
   Remembers the last n-characters appended to it....
*)
module Tail_buffer = struct
  (** remembers the output in a circular buffer.
      looped is used to tell whether we loop around the
      boundary of the buffer.
  *)
  type t = {
    buffer : string;
    length : int;
    mutable looped : bool;
    mutable position : int;
  }

  let contents b =
    if not b.looped then
      String.sub b.buffer ~pos:0 ~len:b.position
    else
      let dst = String.create (b.length + 3) in
      dst.[0] <- '.';
      dst.[1] <- '.';
      dst.[2] <- '.';
      String.blit
        ~src:b.buffer
        ~dst ~dst_pos:3
        ~src_pos:b.position
        ~len:(b.length - b.position);
      String.blit ~src:b.buffer
        ~dst
        ~dst_pos:(b.length - b.position + 3)
        ~src_pos:0
        ~len:(b.position);
      dst

  let create len = {
    buffer = String.create len;
    length = len;
    looped = false;
    position = 0
  }

  let add b src len =
    if b.length <= len then begin
      String.blit
        ~src
        ~dst:b.buffer
        ~dst_pos:0
        ~src_pos:(len - b.length)
        ~len:(b.length);
      b.looped <- true;
      b.position <- 0
    end else
      let leftover =  b.length - b.position in
      if (len < leftover) then begin
        String.blit ~src ~dst:b.buffer ~dst_pos:b.position ~src_pos:0 ~len;
        b.position <- b.position + len;
      end else begin
        String.blit ~src ~dst:b.buffer ~dst_pos:b.position ~src_pos:0
          ~len:leftover;
        b.looped <- true;
        let len = (len-leftover) in
        String.blit ~src ~dst:b.buffer ~dst_pos:0 ~src_pos:leftover ~len;
        b.position <- len
      end
end

module Status = struct
  type t =  [ `User_killed | `Timeout | Unix.Process_status.t ] with sexp_of

  let to_string = function
  | #Unix.Process_status.t as s ->
      Unix.Process_status.to_string_hum s
  | `Timeout     -> "Timed out"
  | `User_killed -> "Killed by the parent"

end
module Command_result = struct
  type t= {
    status: Status.t;
    stdout_tail : string;
    stderr_tail : string
  }
end

(** wait for a given pid to exit;
    returns true when the process exits and false if the process is still runing
    after waiting for [span]
*)
let wait_for_exit ?(is_child=false) span pid =
  let end_time = Time.add (Time.now ()) span in
  let exited () =
    if is_child then
      let mode = [Unix.WNOHANG] in
      match Unix.waitpid ~restart:true ~mode pid with
      | 0,_ -> true
      | v,_ -> assert (v=pid); false
    else
      match Signal.send Signal.zero ~pid with
      | `Ok -> true
      | `No_such_process -> false
  in
  let rec loop () =
    if Time.(>) (Time.now ()) end_time then
      false
        (*We need to explicitely waitpid the child otherwise we are sending
          signals to a zombie*)
    else if not (exited ()) then true
    else begin
      Time.pause (Time.Span.of_sec 0.1);
      loop ()
    end
  in
  loop ()

let kill ?is_child ?(wait_for=Time.Span.of_sec 2.0) ?(signal=Signal.term) pid =
  Signal.send_i ~pid signal;
  if not (wait_for_exit ?is_child wait_for pid) then begin
    Signal.send_i ~pid Signal.kill;
    if not (wait_for_exit wait_for pid) then begin
      failwithf "Process.kill failed to kill %i \
             (or the process wasn't collected by its parent)"
        pid
        ()
    end
  end

(*
  set cwd to [dir] while running [f]
*)

let pushd dir ~f =
  let owd = Sys.getcwd () in
  protectx
    ~finally: (fun _ -> Sys.chdir owd)
    ~f:(fun x ->
          Sys.chdir dir;
          f x)

let close_pooled pool fd =
  if List.mem ~set:!pool fd then
    Unix.close fd;
  pool := List.filter ~f:((<>) fd) !pool

type fd = Unix.file_descr

type t = {
  in_pool        : fd list ref;
  out_pool       : fd list ref;
  buf            : String.t;
  in_cnt         : String.t;
  in_len         : int;
  out_callbacks  : (fd*(string -> int -> unit)) list;
  pid            : int;
  mutable in_pos : int
}

let process_io ~read ~write state =
  List.iter write
    ~f:(fun fd ->
          (try
             let len =
               Unix.single_write fd
                 ~buf:state.in_cnt
                 ~pos:state.in_pos
                 ~len:(state.in_len - state.in_pos)
             in
             state.in_pos <- state.in_pos + len;
             (* Close the process's in_channel iff we are done writing to it*)
             if len = 0 then
               close_pooled state.in_pool fd
           with Unix.Unix_error (Unix.EPIPE,_,_) ->
             close_pooled state.in_pool fd));
  List.iter read
    ~f:(fun fd ->
          let len = Unix.read fd
            ~buf:state.buf
            ~pos:0
            ~len:(String.length state.buf)
          in
          if len = 0 then
            close_pooled state.out_pool fd
          else
            let callback = List.Assoc.find_exn state.out_callbacks fd in
            try
              callback state.buf len
            with Exit -> close_pooled state.out_pool fd)

let available_fds state ~timeout =
  let { Unix.Select_fds.read = read; write = write } =
    Unix.select ()
      ~read:(!(state.out_pool))
      ~write:(!(state.in_pool))
      ~except:[]
      ~restart:true
      ~timeout
  in
  read,write

let create_process ~use_extra_path ~working_dir ~prog ~args ~env =
  let prog = Shell__core.path_expand ?use_extra_path prog in
  let run () = Unix.create_process_env ~env ~prog ~args in
  match working_dir with
  | None -> run ()
  | Some dir -> pushd dir ~f:run ()

let create
    ~use_extra_path
    ~working_dir
    ~prog
    ~args
    ~stdoutf
    ~stderrf
    ~input_string
    ~env =
  let process_info =
    create_process ?use_extra_path ?working_dir ~env ~prog ~args
  in

  let out_fd = process_info.Unix.Process_info.stdout
  and in_fd = process_info.Unix.Process_info.stdin
  and err_fd = process_info.Unix.Process_info.stderr
  and pid = process_info.Unix.Process_info.pid in

  {
    in_pool  = ref [in_fd];
    out_pool = ref [err_fd;out_fd];
    buf      = String.create 4096;
    in_cnt   = input_string;
    in_pos   = 0;
    in_len   = String.length input_string;
    out_callbacks = [out_fd,stdoutf;
                     err_fd,stderrf];
    pid      = pid
  }

let rec finish_reading state =
  match available_fds state ~timeout:0. with
  | []  ,_ -> ()
  | read,_ ->
      process_io state ~read ~write:[];
      finish_reading state

let rec run_loop ~start_time ~timeout state =
  let read,write = available_fds state ~timeout:0.1 in
  process_io state ~read ~write;
  let elapsed = Time.diff (Time.now ()) start_time in
  match timeout with
  | Some timeout when Time.Span.(>) elapsed timeout ->
    kill ~is_child:true state.pid;
    `Timeout
  | None | Some _ ->
    match Unix.waitpid ~mode:[Unix.WNOHANG] state.pid with
    | (0,_) ->
      run_loop ~start_time ~timeout state
    | (_, status) ->
      (status :> Status.t)

let run
    ?timeout
    ?use_extra_path
    ?working_dir
    ?(env=`Extend [])
    ?input:(input_string="")
    ?(stdoutf=(fun _string _len -> ()))
    ?(stderrf=(fun _string _len -> ()))
    ?(tail_len = 2048) ~prog ~args
    () =
  let stdout_tail = Tail_buffer.create tail_len
  and stderr_tail = Tail_buffer.create tail_len in
  let stdoutf sbuf len =
    stdoutf sbuf len;
    Tail_buffer.add stdout_tail sbuf len
  and stderrf sbuf len =
    stderrf sbuf len;
    Tail_buffer.add stderr_tail sbuf len
  in
  let status =
    protectx (Signal.signal Signal.pipe `Ignore,
              create
                ~use_extra_path
                ~working_dir
                ~stderrf
                ~stdoutf
                ~prog
                ~args
                ~env
                ~input_string)
      ~f:(fun (_old_sigpipe,state) ->
            run_loop state
              ~start_time:(Time.now ())
              ~timeout)
      ~finally:(fun (old_sigpipe,state) ->
                  finish_reading state;
                  List.iter (!(state.out_pool)@ !(state.in_pool))
                    ~f:Unix.close;
                  Signal.set Signal.pipe old_sigpipe)
  in
  {Command_result.
     status      = status;
     stdout_tail = Tail_buffer.contents stdout_tail;
     stderr_tail = Tail_buffer.contents stderr_tail }
