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
open Core_extended.Std
open OUnit

(*
  This test checks that no new link dependencies have been added
*)
let libs pgm =
  Shell.run_lines "ldd" [pgm]
  |! List.map ~f:(String.strip)
  |! List.filter
      ~f:(fun s -> not (String.is_prefix ~prefix:"/lib64/ld-linux" s))
  |! List.map
      ~f:(fun s -> match String.lsplit2 s ~on:'.' with
          | None ->
              assert_failure
                (sprintf
                   "ldd_test:%s does not seem to be a valid library name"
                   s)
          | Some (v,_) -> v)

let whitelist = []

let core_hello = ref "core_hello.exe"
let core_extended_hello = ref "core_extended_hello.exe"
let args =
  ["--core-hello",Arg.Set_string core_hello,"PGM hello world program linked against core";
   "--core-extended-hello",Arg.Set_string core_extended_hello,"PGM hello world program linked against core_extended"
  ]

let check_exe f =
  if not (Sys.file_exists_exn f) then
    assert_failure
      (sprintf "could not find "^f)

let test =
  "Ldd_test" >::
    (fun () ->
       check_exe !core_hello;
       check_exe !core_extended_hello;
       let base_libs = libs !core_hello @ whitelist
       and ext_libs = libs  !core_extended_hello in
       let added_libs = List.filter ext_libs
         ~f:(fun l -> not (List.mem ~set:base_libs l))
       in
       if added_libs <> [] then
         assert_failure
           (sprintf "Core_extended links in new external libraries %s"
              (String.concat ~sep:" " added_libs)));
