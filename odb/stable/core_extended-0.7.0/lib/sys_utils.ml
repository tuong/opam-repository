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

(* editors list approximately ordered by ascending new-user-hostility *)
let get_editor () =
  let editors =
    Option.to_list (Sys.getenv "EDITOR")
    @ Option.to_list (Sys.getenv "VISUAL")
    @ ["nano";"emacs"; "vim"; "vi"; "ed"]
  in
  let rec first_valid = function
    | [] -> None
    | ed::eds -> match Shell.which ed with
      | Some _ as e -> e
      | None -> first_valid eds
  in first_valid editors
;;

let digest file =
  if Sys.file_exists file <> `No then
    Digest.file file
  else
    ""

(** Safe editions *)
let edit_one file =
  (** Get the digest of the file we are editing. This is used to test for
      modifications on the file.*)
  let md5 = digest file in
  let editor =
      match get_editor () with
      | Some v -> v
      | None   -> failwithf "Could find an not find a suitable editor" ()
  in
  let pid =
    Extended_unix.unsafe_create_process
      editor
      [file]
  in
  let _,ret = Unix.waitpid ~restart:true ~mode:[] pid in
  match ret with
    (** We have to discard the return value because different editors have
        different conventions... *)
  | `Exited _ ->
      let new_digest = Digest.file file in
      if new_digest = md5 then
        `Unchanged
      else
        `Ok
  | (`Signaled _ | `Stopped _) as v -> v

let rec edit_until check file =
  let relaunch () =
    match Readline.choice ["retry",`Retry;"abort",`Abort] with
    | Some `Retry -> edit_until check file
    | Some `Abort | None -> `Abort
  in
  match edit_one file with
  | `Ok          ->
      begin
        match check file with
        | None -> `Ok
        | Some v ->
            printf "There were errors while validating the file:\n%s\n" v;
            relaunch ()
      end
  | `Unchanged   -> `Unchanged
  | #Unix.Process_status.t as s ->
      printf "The editor died unexpectedly (%s)\n"
        (Unix.Process_status.to_string_hum s);
      relaunch ()

let checked_edit ?(create=false) ~check file =
  (* Blow up early if the file is not accessible *)
  let dirname,basename = Filename.split file in
  let exists = Sys.file_exists_exn file in
  if create && not exists then
    Unix.access dirname ~perm:[Unix.R_OK;Unix.W_OK]
  else
    (* This will blow up if the file does not exist*)
    Unix.access file ~perm:[Unix.R_OK;Unix.W_OK];
  let main,ext = match String.rsplit2 ~on:'.' basename with
    | Some (main,ext) -> (main,"."^ext)
    | None -> basename,""
  in
  let original_digest = digest file in
  let tmp_file = Filename.temp_file main ext in
  let edit_status =
    try
      if exists then
        Shell.cp file tmp_file;
      edit_until check tmp_file
    with e ->
      Shell.rm tmp_file;
      raise e
  in
  match edit_status with
    | `Abort     ->
      Shell.rm tmp_file;
      `Abort
    | `Ok        ->
      if digest file <> original_digest then
        failwithf "The underlying file changed while we were editing it.\
 your version is saved as: %S" tmp_file ();
      Shell.mv tmp_file file;
      `Ok
    | `Unchanged ->
      Shell.rm tmp_file;
      `Ok


module Sexp_checked_edit (S:Sexpable) = struct
  let check file =
    try ignore (Sexp.load_sexp_conv_exn file S.t_of_sexp : S.sexpable); None
    with exc -> Some (Extended_exn.to_string_hum exc)

  let edit = checked_edit ~check
end
