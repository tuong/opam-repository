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

module String = Core_string
module List = Core_list
open Core_printf

include struct
  open Caml.Filename

  let check_suffix = check_suffix
  let chop_extension = chop_extension
  let chop_suffix = chop_suffix
  let current_dir_name = current_dir_name
  let is_implicit = is_implicit
  let is_relative = is_relative
  let parent_dir_name = parent_dir_name
  let dir_sep = dir_sep
  let quote = quote
  let temp_dir_name = temp_dir_name
end

let is_absolute p = not (is_relative p)

(* Pathname resolution *)
external realpath : string -> string = "unix_realpath"

let concat p1 p2 =
  if p1 = "" then
    failwithf "Filename.concat called with an empty string as its first \
              argument (second argument: %s)"
      p2
      ();
  let rec collapse_trailing s =
    match String.rsplit2 s ~on:'/' with
    | Some ("", ("." | "")) -> ""
    | Some (s , ("." | "")) -> collapse_trailing s
    | None | Some _ -> s
  in let rec collapse_leading s =
    match String.lsplit2 s ~on:'/' with
    | Some (("." | ""), s) -> collapse_leading s
    | Some _ | None -> s
  in
  collapse_trailing p1 ^ "/" ^ collapse_leading p2

(* Finds the largest index i in [s] that is less than [from] and for which
   [f s.[i]]
   returns true. Then it returns [i+1]. Raises an exception if [from] isn't a
   valid index
   in [s]. *)
let string_rexists s ~f ~from:n =
  let rec loop n =
    if n = 0 then
      None
    else if f s.[n - 1] then
      Some n
    else
      loop (n - 1)
  in
  loop n

let rec skip_end_slashes s ~from =
  match string_rexists s ~from ~f:(fun c -> c <> '/') with
  | Some v -> `Ends_at v
  | None   -> `All_slashes

(*
  Fix for #0004549. (in the inria bug tracker)
*)
let split = function
  | "" -> ".", "."
  | s ->
      match skip_end_slashes s ~from:(String.length s) with
      | `All_slashes -> "/", "/"
      | `Ends_at basename_end ->
          match string_rexists s ~f:(fun c -> c = '/') ~from:basename_end with
          | None -> ".", String.sub ~pos:0 ~len:basename_end s
          | Some basename_start ->
              let basename =
                String.sub s ~pos:basename_start
                  ~len:(basename_end - basename_start)
              in
              let dirname =
                match skip_end_slashes s ~from:basename_start with
                | `All_slashes -> "/"
                | `Ends_at dirname_end -> String.sub ~pos:0 ~len:dirname_end s
              in
              dirname, basename

(*
  http://www.opengroup.org/onlinepubs/9699919799/utilities/basename.html
  http://www.opengroup.org/onlinepubs/9699919799/utilities/dirname.html
*)
let basename path = snd (split path)
let dirname path = fst (split path)

(* [max_pathname_component_size] comes from getconf _POSIX_NAME_MAX / *)
let max_pathname_component_size = 255

let is_posix_pathname_component s =
  let module S = String in
  s <> "."
  && s <> ".."
  && 0 < S.length s && S.length s <= max_pathname_component_size
  && not (S.contains s '/')
  && not (S.contains s '\000')


let prng = Random.State.make_self_init ()

(* try up to 1000 times to not get a Sys_error when opening a temp
   file / name: *)
let retry ?(in_dir=temp_dir_name) ~f prefix suffix =
  let escape s =
    String.map s ~f:(function
                       | '/' | '\'' | '\000' | '\n' | '-' -> '_'
                       | c -> c)
  in
  let prefix = escape prefix in
  let suffix = escape suffix in
  let rec try_name counter =
    let name =
      if counter = 0 then
        prefix ^ suffix
      else
        let rnd = Random.State.bits prng land 0xFF_FFFF in
        (Printf.sprintf "%s%06x%s" prefix rnd suffix)
    in
    let name = concat in_dir name in
    try
      f name
    with Sys_error _ | Unix.Unix_error _ as e ->
      if counter >= 1000 then raise e else try_name (counter + 1)
  in
  try_name 0

let open_temp_mode = [Open_wronly; Open_creat; Open_excl]

(* these functions are the same as the ones in the std lib but you
   can override the temporary directory you are working in.  They also try the
   exact filename specified by the user before reverting to the "try with"
   machinery.
*)

let temp_dir ?(perm=0o700) ?in_dir prefix suffix =
  retry ?in_dir prefix suffix
    ~f:(fun name -> Unix.mkdir name perm; name)

let open_temp_file ?(perm=0o600) ?in_dir prefix suffix =
  retry ?in_dir prefix suffix
    ~f:(fun name -> (name, open_out_gen open_temp_mode perm name))

let temp_file ?perm ?in_dir prefix suffix =
  let (name, oc) = open_temp_file ?perm ?in_dir prefix suffix in
  close_out oc;
  name

let root = "/"

let split_extension fn =
  match String.rsplit2 ~on:'.' fn with
  | None -> (fn, None)
  | Some (base_fn, ext) -> (base_fn, Some ext)

