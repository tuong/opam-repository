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

let extra_path = ref ["/bin";"/usr/bin";"/usr/local/bin"]

let get_path ?(use_extra_path=true) () =
  let env_path =
    Sys.getenv "PATH"
    |! Option.map ~f:(String.split ~on:':')
    |! Option.value ~default:[]
    |! List.filter ~f:(( <> ) "")
  in
  let path = if use_extra_path then
      env_path @ !extra_path
    else
      env_path
  in
  List.stable_dedup path

let is_executable path =
  try
    let stat = Unix.stat path in
    stat.Unix.st_kind = Unix.S_REG (* Is file *)
    && (stat.Unix.st_perm land 0o111 > 0) (* Is executable*)
  with
  | Unix.Unix_error
      ((Unix.ENOENT|Unix.ENOTDIR), _, _) -> false (* File not found *)

let path_lookup ?use_extra_path bin =
  let rec loop = function
    | [] -> None
    | h::t ->
        let file = h ^/ bin in
        try
          if is_executable file then
            Some file
          else
            raise Exit
        with (Unix.Unix_error _) | Exit ->
          loop t
  in loop (get_path ?use_extra_path ())

let which ?use_extra_path bin =
  if not (String.contains bin '/') then
    path_lookup ?use_extra_path bin
  else begin
    if not (is_executable bin) then
      None
    else
      Some bin
  end

let path_expand ?use_extra_path prog =
  if not (String.contains prog '/') then
    match path_lookup ?use_extra_path prog with
    | None -> failwithf "executable %s not found in $PATH (%s)"
      prog
      (String.concat ~sep:":" (get_path ()))
      ()
    | Some v -> v
  else if Filename.is_relative prog then
    Sys.getcwd () ^/ prog
  else
    prog

(* "real" switches between real and effective uids. sudo sets both real and
   effective uids, so this will not work, though you should be able to use
   $SUDO_UID *)
let whoami ?(real=false) () =
  let uid = if real then Unix.getuid () else Unix.geteuid () in
  
  match Unix.Passwd.getbyuid uid with
  | Some user -> user.Unix.Passwd.name
  | None -> failwith "unable to determine username"
