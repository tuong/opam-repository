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

(* TODO: Ron wants the ability to run interactive commands and to expose the fd version of process
   handling.*)
open Core.Std

include (Shell__core:sig val extra_path : string list ref end)


(* set cwd to [dir] while running [f] *)
(* merge with Process*)
let pushd dir ~f =
  let owd = Sys.getcwd () in
  protectx
    ~finally: (fun _ -> Sys.chdir owd)
    ~f:(fun x ->
          Sys.chdir dir;
          f x
       )

module Process = struct
  (* Same thing as Signal.t but actually has a somewhat human readable output... *)
  type signal = Signal.t
  let sexp_of_signal s = Sexp.Atom (Signal.to_string s)

  type status = [ `Timeout
  | `Exited of int
  | `Signaled of signal
  | `Stopped of signal
 ] with sexp_of

  (* This type is just here to ensure the type status is actualy what we want
     ...*)
  type status_constraint =
      [ `Timeout | Unix.Process_status.t ] as 'a constraint 'a = status

  type t = {
      program   : string;
      arguments : string list;
  } with sexp_of

  type result = {
    command : t;
    status  : status;
    stdout  : string;
    stderr  : string;
  } with sexp_of

  exception Failed of result with sexp

  let to_string {program=prog; arguments=args} =
    let f s =
      if not (String.contains s ' ') &&
        not (String.contains s '"') then
        s
      else
        sprintf "%S" s
    in
    String.concat ~sep:" " (List.map ~f (prog::args))

  let status_to_string s = Process.Status.to_string (s :> Process.Status.t)

  let format_failed c =
    String.concat ~sep:" " ["Command failed:";
                            to_string c.command;
                            "Exit status:";
                            status_to_string c.status;
                            "stderr:";
                            c.stderr]

  module Defaults = struct
    let timeout = ref None
    let verbose = ref false
    let echo = ref false
  end

  let set_defaults ?timeout ?verbose ?echo () =
    Option.iter ~f:(fun v -> Defaults.verbose := v) verbose;
    Option.iter ~f:(fun v -> Defaults.timeout := v) timeout;
    Option.iter ~f:(fun v -> Defaults.echo := v)    echo


  let cmd program arguments = {
    program   = program;
    arguments = arguments;
  }

  
  let shell s =
    {
      program   = "/bin/bash";
      arguments = [ "-c" ; s ]
    }

  (* avoid asking for the password at all costs. *)
  let noninteractive_ssh_options = ["-o";"BatchMode yes"]

  (* Passes the remote command to ssh *)
  
  let remote ?user ~host cmd   =
    let url = match user with
      | None      -> host
      | Some user -> user ^"@"^host
    in
    { program   = "/usr/bin/ssh";
      arguments =
        noninteractive_ssh_options @ [url] @
          (* We need to escape all the arguments because ssh is passing this to
             the remote shell which will unescape all of that before passing it
             over to our program.*)
          List.map ~f:Filename.quote (cmd.program::cmd.arguments)
    }


  type 'res acc =
      { add           : string -> int -> unit;
        flush         : unit -> 'res; }

  type 'res reader = unit -> 'res acc

  let run' ~use_extra_path ~timeout ~working_dir ~verbose ~echo ~input ~export
      ~f cmd
      =
    let export = Option.value export ~default:[] in
    let verbose = Option.value verbose ~default:(!Defaults.verbose) in
    let timeout = Option.value timeout ~default:(!Defaults.timeout) in
    let echo    = Option.value echo    ~default:(!Defaults.echo)    in
    if echo then
      Console.Ansi.printf [`Underscore] "Shell: %s\n%!" (to_string cmd);
    let stderrf =
      if verbose then
        (fun s len -> Console.Ansi.output [`Red] stderr s 0 len)
      else
        (fun _s _len -> ())
    and stdoutf =
      if verbose then
        (fun s len ->
           Console.Ansi.output [`Green] stderr s 0 len;
          f s len)
      else f
    in
    Process.run ?timeout ?input ?working_dir ?use_extra_path
      ~stdoutf
      ~stderrf
      ~env:(`Extend export)
      ~prog:cmd.program
      ~args:cmd.arguments
      ()

  let run ?use_extra_path ?timeout ?working_dir ?export ?expect ?verbose ?echo
      ?input cmd
      (reader:_ reader) =
    let expect = Option.value expect ~default:[0] in
    let acc = reader () in
    let early_exit = ref false in
    let r = run'
      ~use_extra_path
      ~timeout
      ~working_dir
      ~verbose
      ~echo
      ~input
      ~export
      cmd
      ~f:(fun s len ->
            try
              acc.add s len
            with Exit ->
              early_exit := true;
              raise Exit)
    in
    let module Res = Process.Command_result in
    match r.Res.status with
    | _ when !early_exit -> acc.flush ()
    | `Exited i when List.mem i ~set:expect -> acc.flush ()
    | `User_killed -> assert false
    | #status as status ->
        raise (Failed
                 { command = cmd;
                   status  = (status :> status);
                   stderr  = r.Res.stderr_tail;
                   stdout  = r.Res.stdout_tail })

  let test ?use_extra_path ?timeout ?working_dir ?export ?verbose ?echo ?input
      ?(true_v=[0])
      ?(false_v=[1]) cmd  =
    let r =
      run'
        ~export
        ?use_extra_path
        ?timeout ?working_dir ?verbose ?echo ?input cmd ~f:(fun _ _ -> ())
    in
    let module Res = Process.Command_result in
    match r.Res.status with
    | `Exited i when List.mem i ~set:true_v -> true
    | `Exited i when List.mem i ~set:false_v -> false
    | `User_killed -> assert false
    | #status as status ->
        raise (Failed
          {
            command = cmd;
            status  = (status :> status);
            stderr  = r.Res.stderr_tail;
            stdout  = r.Res.stdout_tail
          })

  let discard () = { add   = (fun _ _ -> ()); flush = (fun () -> ()) }

  let content () =
    let buffer = Buffer.create 16 in
    {
      add   = (fun s len -> Buffer.add_substring buffer s 0 len);
      flush = (fun () -> Buffer.contents buffer);
    }

  let strip_trailing_nl str =
    if str <> "" && str.[String.length str - 1] = '\n'
    then String.drop_suffix str 1
    else str

  let lines () =
    let buffer = Buffer.create 16 in
    {
      add   = (fun s len -> Buffer.add_substring buffer s 0 len);
      flush = (fun () ->
                 match Buffer.contents buffer with
                 | "" -> []
                 | s -> String.split ~on:'\n' (strip_trailing_nl s)
              )
    }

  let head () =
    let buffer = Buffer.create 16 in
    {
      add   = (fun s len ->
        begin try
          let e = String.index_exn s '\n' in
          if e < len then begin
            Buffer.add_substring buffer s 0 e;
            raise Exit
          end
          with
          | Not_found -> ()
        end;
        Buffer.add_substring buffer s 0 len
      );
      flush = (fun () -> Buffer.contents buffer) }
end

type 'a with_process_flags =
    ?use_extra_path:bool
  -> ?timeout:Time.Span.t option
  -> ?working_dir:string (* rename to run_in? *)
  -> ?export:(string*string) list
  -> ?verbose:bool
  -> ?echo:bool
  -> ?input:string
  -> 'a

type 'a with_run_flags =
     (* Defaults to [0]*)
    (?expect:int list -> 'a) with_process_flags

type 'a with_test_flags =
    (?true_v:int list -> ?false_v:int list ->'a ) with_process_flags

type 'a cmd = string -> string list -> 'a

type ('a,'ret) sh_cmd = (('a, unit, string,'ret) format4 -> 'a)

let run_gen reader ?use_extra_path ?timeout ?working_dir ?export ?verbose ?echo
    ?input ?expect cmd args
    =
  Process.run
    ?use_extra_path
    ?timeout
    ?working_dir
    ?export
    ?expect
    ?verbose
    ?echo
    ?input
    (Process.cmd cmd args)
    reader

let run = run_gen Process.discard
let run_lines = run_gen Process.lines
let run_one  = run_gen Process.head
let run_full = run_gen Process.content

let test ?use_extra_path ?timeout ?working_dir ?export ?verbose ?echo ?input
    ?true_v ?false_v cmd args =
  Process.test
    ?use_extra_path
    ?timeout
    ?working_dir
    ?export
    ?verbose
    ?echo
    ?input
    ?true_v
    ?false_v
    (Process.cmd cmd args)

let sh_gen reader ~use_extra_path ~timeout ~working_dir ~export ~verbose ~echo
    ~input ~expect
    fmt =
  ksprintf (fun s -> Process.run
    ?use_extra_path
    ?timeout
    ?working_dir
    ?expect
    ?export
    ?verbose
    ?echo
    ?input
    (Process.shell s)
    reader
  )fmt


(* We need the eta expansion here for the format :-( *)
let sh ?use_extra_path ?timeout ?working_dir ?export ?verbose ?echo ?input
    ?expect fmt
    =
  sh_gen Process.discard ~use_extra_path ~timeout ~working_dir ~export ~expect
    ~verbose ~echo ~input fmt

let sh_one ?use_extra_path ?timeout ?working_dir ?export ?verbose ?echo ?input
    ?expect fmt =
  sh_gen Process.head ~use_extra_path ~timeout ~working_dir ~export ~expect
    ~verbose ~echo ~input fmt

let sh_lines ?use_extra_path ?timeout ?working_dir ?export  ?verbose ?echo
    ?input ?expect fmt =
  sh_gen Process.lines ?use_extra_path
    ~timeout ~working_dir ~export ~expect ~verbose ~echo ~input fmt

let sh_full ?use_extra_path ?timeout ?working_dir ?export ?verbose ?echo ?input
    ?expect fmt =
  sh_gen Process.content ~use_extra_path ~timeout ~export ~working_dir ~expect
    ~verbose ~echo ~input fmt

let sh_test ?use_extra_path ?timeout ?working_dir ?export ?verbose ?echo ?input
    ?true_v ?false_v fmt =
  ksprintf (fun s -> Process.test
    ?use_extra_path
    ?timeout
    ?working_dir
    ?export
    ?verbose
    ?echo
    ?input
    ?true_v
    ?false_v
    (Process.shell s)
  ) fmt

type ('a,'ret) ssh_cmd =
    ?user:string -> host:string -> ('a, unit, string,'ret) format4 -> 'a


let noninteractive_ssh_options = Process.noninteractive_ssh_options


let ssh_gen reader ~use_extra_path ~timeout ~working_dir ~export ~verbose ~echo
    ~input ~expect ~user ~host fmt =
  ksprintf (fun s -> Process.run
              ?use_extra_path
              ?timeout
              ?working_dir
              ?expect
              ?export
              ?verbose
              ?echo
              ?input
              (Process.remote ?user ~host (Process.shell s))
              reader)
    fmt

let ssh ?use_extra_path ?timeout ?working_dir ?export ?verbose ?echo ?input
    ?expect ?user ~host
    fmt =
  ssh_gen Process.discard ~use_extra_path ~timeout ~working_dir ~export ~expect
    ~verbose ~echo ~user ~host ~input fmt

let ssh_one ?use_extra_path ?timeout ?working_dir ?export ?verbose ?echo ?input
    ?expect ?user ~host fmt =
  ssh_gen Process.head ~use_extra_path ~timeout ~working_dir ~export ~expect
    ~verbose ~echo ~user ~host ~input fmt

let ssh_lines ?use_extra_path ?timeout ?working_dir ?export  ?verbose ?echo
    ?input ?expect ?user ~host fmt =
  ssh_gen Process.lines ~use_extra_path ~timeout ~working_dir ~export ~expect
    ~verbose ~echo ~user ~host ~input fmt

let ssh_full ?use_extra_path ?timeout ?working_dir ?export ?verbose ?echo ?input
    ?expect ?user ~host fmt =
  ssh_gen Process.content ~use_extra_path ~timeout ~export ~working_dir ~expect
    ~verbose ~echo ~user ~host ~input fmt

let ssh_test ?use_extra_path ?timeout ?working_dir ?export ?verbose ?echo ?input
    ?true_v ?false_v ?user ~host fmt =
  ksprintf (fun s -> Process.test
    ?use_extra_path
    ?timeout
    ?working_dir
    ?export
    ?verbose
    ?echo
    ?input
    ?true_v
    ?false_v
    (Process.remote ?user ~host (Process.shell s))
  ) fmt
;;

let whoami = Shell__core.whoami
let which = Shell__core.which

let rm ?r ?f path =
  let r = Option.map r ~f:(fun () -> "-r") in
  let f = Option.map f ~f:(fun () -> "-f") in
  run "/bin/rm" (List.filter_map ~f:ident [r; f; Some "--"; Some path])

let mv src dst =
  run "/bin/mv" ["--";src;dst]

let mkdir ?p ?perm path =
  let p = Option.map p ~f:(fun () -> "-p") in
  let mode = Option.map perm ~f:(sprintf "--mode=%o") in
  run "/bin/mkdir" (List.filter_map ~f:ident [p; mode;Some "--";Some path])

(* TODO: Deal with atomicity  *)


let cp ?(overwrite=true) ?perm src dst =
  let perm = match perm with
    | Some p -> p
    | None -> (Unix.lstat src).Unix.st_perm
  in
  let dst = if Sys.is_directory dst = `Yes then
      dst ^/ (Filename.basename src)
    else
      dst
  in
  let out_mode =
    if overwrite then
      [ Unix.O_WRONLY; Unix.O_NOCTTY; Unix.O_CREAT; Unix.O_TRUNC ]
    else
      [ Unix.O_WRONLY; Unix.O_NOCTTY; Unix.O_CREAT; Unix.O_EXCL ]
  in
  protectx (Unix.openfile src ~mode:[ Unix.O_RDONLY; Unix.O_NOCTTY ] ~perm:0)
    ~f:(fun infh ->
          protectx (Unix.openfile dst ~mode:out_mode ~perm)
            ~f:(fun outfh ->
                  let buflen = 4096 in
                  let buf = String.create buflen in
                  let rec loop () =
                    let rlen = Unix.read infh ~buf ~pos:0 ~len:buflen in
                    if rlen <> 0 then
                      let wlen = Unix.write outfh ~buf ~pos:0 ~len:rlen in
                      if rlen <> wlen then
                        failwithf "Short write: tried to write %d bytes, \
                                   only wrote %d bytes" rlen wlen ();
                      loop ()
                  in
                  loop ();
               )
            ~finally:Unix.close
       )
    ~finally:Unix.close
;;

let scp ?(recurse=false) ?user ~host f t =
  let user_arg = Option.value_map user ~default:"" ~f:(fun user -> user ^ "@") in
  let args = [f; user_arg ^ host ^ ":" ^ t] in
  let args = if recurse then "-r"::args else args in
  run "scp" args
;;
