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

(** Shell scripting in OCaml.

    This module contains basic blocks for shell scripting in OCaml. It tends to
    be safer than just using [Unix.system] because it handles errors more
    strictly.
*)
open Core.Std

(** {6 Process handling }  *)
(**
   This type is an umbrella type for all the command that dispatch a process.
   It comes with a list of arguments whose default value can be tweaked by
   set_defaults.

   - [use_extra_path] : if we fail to find the command in the path then
   we look for it [extra_path]
   - [timeout] : the command will raise [Failed] if the program doesn't
     do any IO for this period of time
   - [working_dir] : run the command in this directory
   - [verbose] : prints the output of the command
   - [echo] : print out the command before running it
   - [input] : a string to pipe through the program's standard in
   - [export] : a list of variable to export in the environement of the
   dispatched programm
*)
type 'a with_process_flags =
  ?use_extra_path:bool
  -> ?timeout:Time.Span.t option
  -> ?working_dir:string (* rename to run_in? *)
  
  -> ?export:(string*string) list
  -> ?verbose:bool
  -> ?echo:bool
  -> ?input:string
  -> 'a

(**
   This is the list of flags for normal process dispatch. It is an extension of
   [with_process_flags].

   - [expect] : an int list of valid return codes. default value is [[0]], if
   the return code of the dispatched is not in this list we will blowup with
   [Process.Failure]
*)
type 'a with_run_flags = (?expect:int list ->'a ) with_process_flags

(** {9 Basic run functions}

    In all the functions below the command is specified with two arguments.  The
    first one is a string representing the process to run.  The second one is
    the list of arguments to pass.

    Although the arguments do not need to be escaped there is still a risk that
    they might be interpreted as flags when they aren't. Most basic unix
    utilities provide the ability to pass arguments after "--" to avoid this.

    Usage example:
    {[
    let patch = run_full ~expect:[0;1] "diff" ["-u";"--";file1;file2]
    ]}
*)

type 'a cmd = string -> string list -> 'a

(** Runs a command and discards its output. *)
val run       : unit cmd with_run_flags

(** Runs a command and returns its output line separated. Note: most commands
    print a newline at the end of their output so the shell prompt appears on
    its own line. If the output ends in a newline, it is stripped before
    splitting the output into a string list to avoid there being a final
    element in the list containing just the empty string.

    In some cases, the newline should not be stripped (e.g., "cat" will not
    "add" a newline). If you care, use [run_full] for the entire buffer.
*)
val run_lines : string list cmd with_run_flags

(** Returns the first line of the command's output.
    (This function might terminate the program early the same way that
    piping through grep would)
*)
val run_one   : string cmd with_run_flags

(** Return the full command's output in one string. See the note in
    [run_lines].
*)
val run_full  : string cmd with_run_flags


(** {9 Dispatch to /bin/bash}

    All these function take a format (like printf) and run it through the shell.

    Usage example:
{[
    sh "cp -- %s %s" (Filename.quote file1)  (Filename.quote file2)
]}

    In general it is recommended to avoid using those too much and to prefer the
    run* family of function instead because it avoids pitfall like escaping
    issues and is much more straightforward to think about.
*)


type ('a,'ret) sh_cmd = ('a, unit, string,'ret) format4 -> 'a

val sh       : ('a,unit)        sh_cmd with_run_flags
val sh_lines : ('a,string list) sh_cmd with_run_flags
val sh_one   : ('a,string)      sh_cmd with_run_flags
val sh_full  : ('a,string)      sh_cmd with_run_flags

(* Magic invocation to avoid asking for password if we can.  These arguments are passed
   to ssh in the [ssh_*] functions below.  They're exposed in case you want to use them
   in a different context. *)
val noninteractive_ssh_options : string list

type ('a,'ret) ssh_cmd =
    ?user:string -> host:string -> ('a, unit, string,'ret) format4 -> 'a

val ssh       : ('a,unit)          ssh_cmd with_run_flags
val ssh_lines : ('a,string list) ssh_cmd with_run_flags
val ssh_one   : ('a,string)      ssh_cmd with_run_flags
val ssh_full  : ('a,string)      ssh_cmd with_run_flags

(** {9 Test dispatches}

    Usage example:
{[
    if Shell.test "diff" ["-q";"--";file1;file2] then
       Printf.printf "Files %S and %S are the same\n%!" file1 file2;
]}

*)

(** This is the list of flags for dispatching processes in test mode. This is
    used to test the return code of the dispatched program. The return value of
    these functions will be :
    - [true] if the exit code is in [true_v].
    - [false] if the exit code is in [false_v] and not in [true_v].
    - Raises [Process.Failure] otherwise

    The default values are:
    - [true_v]: default value [[0]]
    - [false_v]: default_value [[1]]
*)
type 'a with_test_flags = (?true_v:int list -> ?false_v:int list ->'a )
    with_process_flags

val test    : bool cmd with_test_flags

val sh_test : ('a,bool) sh_cmd with_test_flags

val ssh_test : ('a,bool) ssh_cmd with_test_flags

(** variable used by dispatch command to find binaries not in the path.
    The default values contains only directory which should be in PATH and is
    only useful in environments where the PATH variable has been blown away.
*)
val extra_path : string list ref

(** Process dispatching *)
module Process : sig

  type status =  [ `Timeout | Unix.Process_status.t ]
  (** The termination status of a process.
      This is an extension of [Unix.Process_status.t] to allow timeouts.
  *)

  type t

  type result = {
    command : t;
    status  : status;
    stdout  : string;
    stderr  : string
  }

  exception Failed of result

  val to_string        : t -> string
  val status_to_string : status -> string

  val set_defaults :
    ?timeout:Time.Span.t option ->
    ?verbose:bool ->
    ?echo:bool ->
    unit -> unit

  
  val format_failed : result -> string

  val cmd    : string -> string list -> t
  val shell  : string -> t
  val remote : ?user:string -> host:string -> t -> t

  type 'a reader

  val content : string reader
  val discard : unit reader
  val lines   : string list reader
  val head    : string reader


  val run :
    ?use_extra_path:bool
    -> ?timeout:Time.Span.t option
    -> ?working_dir:string
    -> ?export:(string*string) list
    -> ?expect: int list
    -> ?verbose:bool
    -> ?echo:bool
    -> ?input:string
    -> t
    -> 'a reader
    -> 'a

  val test :
    ?use_extra_path:bool
    -> ?timeout:Time.Span.t option
    -> ?working_dir:string
    -> ?export:(string*string) list
    -> ?verbose:bool
    -> ?echo:bool
    -> ?input:string
    -> ?true_v:int list
    -> ?false_v:int list
    -> t
    -> bool
end

(** {6 Small helper commands} *)

val mkdir : ?p:unit -> ?perm:int -> string -> unit


val cp :
  ?overwrite:bool
  -> ?perm:Unix.file_perm
  -> string
  -> string
  -> unit

val rm : ?r:unit -> ?f:unit -> string -> unit

val mv : string -> string -> unit
(** Raises "Failed_command" *)

(** Get the username. By default, the effective username. If real is true, get
    the real username. *)
val whoami : ?real:bool -> unit -> string

val which : ?use_extra_path:bool -> string -> string option

(** [scp user host from to] copy local file from to to *)
val scp : ?recurse:bool -> ?user:string -> host:string -> string -> string -> unit
