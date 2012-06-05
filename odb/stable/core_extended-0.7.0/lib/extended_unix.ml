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
open Unix

external raw_unsafe_create_process :
  prog : string
  -> args : string array
  -> stdin : file_descr
  -> stdout : file_descr
  -> stderr : file_descr
  -> fds_to_close : file_descr array
  -> clear_env : bool
  -> setenv_pairs : (string * string) array
  -> int
  =  "extended_ml_create_process_bc" "extended_ml_create_process"

let unsafe_create_process
    ?(stdin=Unix.stdin)
    ?(stdout=Unix.stdout)
    ?(stderr=Unix.stderr)
    ?(path_lookup=true)
    ?(env=`Add [])
    ?(fds_to_close=[])
    prog
    args
    =
  let clear_env,setenv_pairs =
    match env with
    | `Add l     -> false,l
    | `Replace l -> true,l
  in
  List.iter setenv_pairs
    ~f:(fun (key,_value) ->
      if String.mem key '=' then
        failwithf "extended_unix.unsafe_create_process:\
  variable to export in the environement %S contains an equal sign"
          key
          ());
  let prog =
    if path_lookup then
      match Shell__core.which prog with
      | Some s -> s
      | None -> failwithf "unsafe_create_process: Process not found %s"
        prog
        ()
    else
      prog
  in
  raw_unsafe_create_process
    ~fds_to_close:(Array.of_list fds_to_close)
    ~setenv_pairs:(Array.of_list setenv_pairs)
    ~clear_env
    ~prog
    ~args:(Array.of_list (prog::args))
    ~stdin
    ~stdout
    ~stderr

external seteuid : int -> unit = "extended_ml_seteuid"
external setreuid : uid:int -> euid:int -> unit = "extended_ml_setreuid"
external gettid : unit -> int = "extended_ml_gettid"

(* In case we ever need it here is a create_process implemented on top of
   [unsafe_create_process]
*)
(*
let create_process ~prog ~args =
  let close_on_err = ref [] in
  try
    let (in_read, in_write) = pipe() in
    close_on_err := in_read :: in_write :: !close_on_err;
    let (out_read, out_write) = pipe() in
    close_on_err := out_read :: out_write :: !close_on_err;
    let (err_read, err_write) = pipe() in
    close_on_err := err_read :: err_write :: !close_on_err;
    let pid = unsafe_create_process
      prog
      args
      ~fds_to_close:[in_write;out_read;err_read]
      ~stdin:in_read
      ~stdout:out_write
      ~stderr:err_write
    in
    close in_read;
    close out_write;
    close err_write;
    {
      Process_info.pid = pid;
      stdin = in_write;
      stdout = out_read;
      stderr = err_read
    }
  with e ->
    List.iter ~f:(fun fd -> try close fd with _ -> ()) !close_on_err;
    raise e
*)

type statvfs = {
  bsize: int;                           (** file system block size *)
  frsize: int;                          (** fragment size *)
  blocks: int;                          (** size of fs in frsize units *)
  bfree: int;                           (** # free blocks *)
  bavail: int;                          (** # free blocks for non-root *)
  files: int;                           (** # inodes *)
  ffree: int;                           (** # free inodes *)
  favail: int;                          (** # free inodes for non-root *)
  fsid: int;                            (** file system ID *)
  flag: int;                            (** mount flags *)
  namemax: int;                         (** maximum filename length *)
} with sexp, bin_io

(** get file system statistics *)
external statvfs : string -> statvfs = "statvfs_stub"

(** get load averages *)
external getloadavg : unit -> float * float * float = "getloadavg_stub"

module Extended_passwd = struct
  open Passwd

  let of_passwd_line_exn s =
    match String.split s ~on:':' with
    | name::passwd::uid::gid::gecos::dir::shell::[] ->
        { name = name;
          passwd = passwd;
          uid = Int.of_string uid;
          gid = Int.of_string gid;
          gecos = gecos;
          dir = dir;
          shell = shell
        }
    | _ -> failwithf "of_passwd_line: failed to parse: %s" s ()
  ;;
  let of_passwd_line s = Option.try_with (fun () -> of_passwd_line_exn s) ;;

  let of_passwd_file_exn fn =
    Exn.protectx (In_channel.create fn)
      ~f:(fun chan ->
        List.map (In_channel.input_lines chan) ~f:of_passwd_line_exn)
      ~finally:In_channel.close
  ;;

  let of_passwd_file f = Option.try_with (fun () -> of_passwd_file_exn f) ;;
end

