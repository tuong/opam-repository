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

module LargeFile = Unix.LargeFile

let getenv var = try Some (Sys.getenv var) with Not_found -> None

let getenv_exn var =
  match getenv var with
  | Some x -> x
  | None ->
    Core_printf.failwithf
      "Sys.getenv_exn: environment variable %s is not set" var ()



let stat_check_exn f ?(follow_symlinks=true) path =
  try
    
    (* According to STAT(2) no risk of EINTR. *)
    let  stat =
      if follow_symlinks then
          LargeFile.stat path
        else
          LargeFile.lstat path
    in
    f stat
  with
  | Unix.Unix_error ((Unix.ENOENT|Unix.ENOTDIR), _, _) -> false


let stat_check f ?follow_symlinks path =
  try
    if stat_check_exn f ?follow_symlinks path then
      `Yes
    else
      `No
  with
  | Unix.Unix_error ((Unix.EACCES|Unix.ELOOP), _, _)   -> `Unknown

let file_exists = stat_check (fun _ -> true)
let file_exists_exn = stat_check_exn (fun _ -> true)

let is_directory = stat_check (fun stat -> stat.LargeFile.st_kind = Unix.S_DIR)
let is_directory_exn = stat_check_exn
  (fun stat -> stat.LargeFile.st_kind = Unix.S_DIR)

let is_file = stat_check (fun stat -> stat.LargeFile.st_kind = Unix.S_REG)
let is_file_exn = stat_check_exn
  (fun stat -> stat.LargeFile.st_kind = Unix.S_REG)

include struct
  open Caml.Sys
  let argv = argv
  let executable_name = executable_name
  let remove = remove
  let rename = rename
  let command = command
  let chdir = chdir
  let getcwd = getcwd
  let readdir = readdir
  let interactive = interactive
  let os_type = os_type
  let word_size = word_size
  let max_string_length = max_string_length
  let max_array_length = max_array_length
  exception Break = Break
  let catch_break = catch_break
  let ocaml_version = ocaml_version
end

exception Command_failed_with_status of Core_int.t * Core_string.t with sexp

let command_exn string =
  let status = command string in
  if status <> 0 then raise (Command_failed_with_status (status, string))
;;
