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

open Core.Std;;

external running_byte_code :
  unit -> unit -> unit -> unit -> unit -> unit -> bool
  = "caml_running_byte_code_bc" "caml_running_byte_code_nc" "noalloc"

let running_byte_code () = running_byte_code () () () () () ()

let home () =
  (Unix.Passwd.getbyuid_exn (Unix.geteuid ())).Unix.Passwd.dir

let groups = Memo.unit
  (fun () ->
     Unix.getgroups () |! Array.to_list |!
         List.map ~f:(fun gid ->
                        (Unix.Group.getbygid_exn gid).Unix.Group.name))

let hostname = Unix.gethostname

let file_kind f = (Unix.lstat f).Unix.st_kind

let ls dir = Sys.readdir dir
  |! Array.to_list
  |! List.sort ~cmp:Extended_string.collate
