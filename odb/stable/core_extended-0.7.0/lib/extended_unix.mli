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
(** Extensions to [Core.Unix]. *)

val unsafe_create_process :
  ?stdin:Unix.file_descr ->
  ?stdout:Unix.file_descr ->
  ?stderr:Unix.file_descr ->
  ?path_lookup:bool ->
  ?env:[ `Add of (string * string) list
       | `Replace of (string * string) list ] ->
  ?fds_to_close:Unix.file_descr list ->
  string -> string list -> int
(** [unsafe_create_process prog args ~stdin ~stdout ~stderr]
   forks a new process that executes the program
   in file [prog], with arguments [args]. The pid of the new
   process is returned immediately; the new process executes
   concurrently with the current process.
   The standard input and outputs of the new process are connected
   to the descriptors [stdin], [stdout] and [stderr].
   @param path_lookup if [true] than we use PATH to find the process to exec.
   @env specifies the enviroenment the process runs in
   @fds_to_close a list of file descriptors to close on the forked side.
*)

val seteuid : int -> unit

val setreuid : uid:int -> euid:int -> unit

val gettid : unit -> int

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

module Extended_passwd : sig
  open Unix.Passwd

  (** [of_passwd_line] parse a passwd-like line *)
  val of_passwd_line : string -> t option

  (** [of_passwd_line_exn] parse a passwd-like line *)
  val of_passwd_line_exn : string -> t

  (** [of_passwd_file] parse a passwd-like file *)
  val of_passwd_file : string -> t list option

  (** [of_passwd_file_exn] parse a passwd-like file *)
  val of_passwd_file_exn : string -> t list
end

