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

let load ?pos ?len file bin_read_t =
  let failwith s = failwith ("Core_extended.Bin_io_utils.load: " ^ s) in
  let pos =
    match pos with
    | None -> Int64.zero
    | Some pos when pos < Int64.zero -> failwith "pos < 0"
    | Some pos -> pos
  in
  let bstr =
    read_wrap file ~f:(fun ic ->
      let file_size = In_channel.length ic in
        let len64 =
          match len with
          | None when pos > file_size -> failwith "pos > file size"
          | None -> Int64.(-) file_size pos
          | Some len64 when len64 < Int64.zero -> failwith "len < 0"
          | Some len64 when Int64.(+) pos len64 > file_size ->
              failwith "pos + len < file size"
          | Some len64 -> len64
        in
        let len =
          match Int64.to_int len64 with
          | None -> failwith "len exceeds maximum integer"
          | Some len -> len
        in
        let bstr = Bigstring.create len in
        Bigstring.really_input ic bstr;
        bstr)
  in
  let pos_ref = ref 0 in
  let v = bin_read_t bstr ~pos_ref in
  if !pos_ref <> Bigstring.length bstr then failwith "garbage after data"
  else v

let save ?header ?perm file bin_writer_t v =
  let bstr = Bin_prot.Utils.bin_dump ?header bin_writer_t v in
  let tmp_name, oc =
    let in_dir =
      if Filename.is_relative file then "."
      else Filename.dir_sep
    in
    Filename.open_temp_file ~in_dir file "tmp"
  in
  try
    Bigstring.really_output oc bstr;
    close_out oc;
    let perm =
      match perm with
      | Some perm -> perm
      | None ->
          let umask = Unix.umask 0 in
          ignore (Unix.umask umask);
          umask lxor 0o666
    in
    if perm <> 0o600 then Unix.chmod tmp_name ~perm;
    Sys.rename tmp_name file
  with e ->
    close_out_noerr oc;
    raise e
