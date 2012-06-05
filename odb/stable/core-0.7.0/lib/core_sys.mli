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

(** System interface. *)

(** The command line arguments given to the process.  The first element is the
    command name used to invoke the program.  The following elements are the
    command-line arguments given to the program. *)
val argv : string array

(** The name of the file containing the executable currently running. *)
val executable_name : string



(** For all of the following functions, [?follow_symlinks] defaults to [true]. *)

(** [file_exists ~follow_symlinks path]

   Test whether the file in [path] exists on the file system.
   If [follow_symlinks] is [true] and [path] is a symlink the result concerns
   the target of the symlink.

   [`Unknown] is returned for files for which we cannot successfully determine
   whether they are on the system or not (e.g. files in directories to which we
   do not have read permission).

   [?follow_symlinks] defaults to [true]
*)
val file_exists : ?follow_symlinks:bool -> string ->  [ `Yes | `No | `Unknown ]

(** Same as [file_exists] but blows up on [`Unknown] *)
val file_exists_exn : ?follow_symlinks:bool -> string -> bool

(** Returns [`Yes] if the file exists and is a directory*)
val is_directory : ?follow_symlinks:bool -> string -> [ `Yes | `No | `Unknown ]

(** Returns [`Yes] if the file exists and is a regular file *)
val is_file : ?follow_symlinks:bool -> string      -> [ `Yes | `No | `Unknown ]

val is_directory_exn : ?follow_symlinks:bool -> string -> bool
val is_file_exn : ?follow_symlinks:bool -> string      -> bool

(** Remove the given file name from the file system. *)
val remove : string -> unit

(** Rename a file. The first argument is the old name and the second is the new
    name. If there is already another file under the new name, [rename] may
    replace it, or raise an exception, depending on your operating system. *)

val rename : string -> string -> unit

(** Return the value associated to a variable in the process environment.  Return [None]
    if the variable is unbound.  *)
val getenv : string -> string option
val getenv_exn : string -> string

(** Execute the given shell command and return its exit code. *)
val command : string -> int

(** [command_exn command] runs [command] and then raises an exception if it
    returns with nonzero exit status. *)
val command_exn : string -> unit

(** Change the current working directory of the process. *)
val chdir : string -> unit

(** Return the current working directory of the process. *)
val getcwd : unit -> string

(** Return the names of all files present in the given directory.  Names
    denoting the current directory and the parent directory (["."] and [".."] in
    Unix) are not returned.  Each string in the result is a file name rather
    than a complete path.  There is no guarantee that the name strings in the
    resulting array will appear in any specific order; they are not, in
    particular, guaranteed to appear in alphabetical order. *)
val readdir : string -> string array

(** This reference is initially set to [false] in standalone programs and to
    [true] if the code is being executed under the interactive toplevel system
    [ocaml]. *)
val interactive : bool ref

(** Operating system currently executing the Caml program. One of
    -  ["Unix"] (for all Unix versions, including Linux and Mac OS X),
    -  ["Win32"] (for MS-Windows, OCaml compiled with MSVC++ or Mingw),
    -  ["Cygwin"] (for MS-Windows, OCaml compiled with Cygwin). *)
val os_type : string

(** Size of one word on the machine currently executing the Caml program, in
    bits: 32 or 64. *)
val word_size : int


(** Maximum length of a string. *)
val max_string_length : int


(** Maximum length of a normal array.  The maximum length of a float array is
    [max_array_length/2] on 32-bit machines and [max_array_length] on 64-bit machines. *)
val max_array_length : int

(** Exception raised on interactive interrupt if {!Sys.catch_break} is on. *)
exception Break

(** [catch_break] governs whether interactive interrupt (ctrl-C) terminates the
    program or raises the [Break] exception.  Call [catch_break true] to enable
    raising [Break], and [catch_break false] to let the system terminate the
    program on user interrupt. *)
val catch_break : bool -> unit

(** [ocaml_version] is the version of Objective Caml.  It is a string of the form
    ["major.minor[.patchlevel][+additional-info]"], where [major], [minor], and
    [patchlevel] are integers, and [additional-info] is an arbitrary string. The
    [[.patchlevel]] and [[+additional-info]] parts may be absent. *)
val ocaml_version : string;;
