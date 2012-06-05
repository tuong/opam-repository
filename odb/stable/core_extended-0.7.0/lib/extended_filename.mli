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

(** Extensions to [Core.Core_filename].*)

val normalize : string -> string
(**
   [normalize path]
   Removes as much "." and ".." from the path as possible. If the path is
   absolute they will all be removed.
*)

val parent : string -> string
(**
   [parent path]
   The parent of the root directory is the root directory
   @return the path to the parent of [path].
*)

(**
   [make_relative ~to_:src f]
   returns [f] relative to [src].

   @raise Failure if [is_relative f <> is_relative src]
*)
val make_relative : ?to_:string -> string -> string

(**
   [make_absolute src]
   Turn [src] in an absolute path expanded from the current working directory.
*)
val make_absolute : string -> string

val expand : ?from:string -> string -> string
(**
   [expand]
   Makes a path absolute and expands [~] [~username] to home directories.
   In case of error (e.g.: path home of a none existing user) raises [Failure]
   with a (hopefully) helpful message.
*)



(** Splits a given path in a list of strings. *)
val explode : string -> string list

(** dual to explode *)
val implode : string list -> string


(**/**)
(* this is exported because it is used by core_extended.filename. *)
val normalize_path : string list -> string list
(**/**)


(**
   Filename.compare is a comparaison that normalizes filenames ("./a" = "a") use
   a more human ready algorithm based on [String.collate]
   ("rfc02.txt > rfc1.txt") and extenstions ("a.c" > "a.h").

   It is a total comparaison on normalized filenames.
*)
val compare: string -> string -> int

(**
   [with_open_temp_file ~write ~f prefix suffix]
   create a temporary file; runs [write] on its [out_channel] and then [f] on
   the resulting file. The file is removed once [f] is done running.
*)
val with_open_temp_file:
  ?in_dir: string
  -> ?write:(out_channel -> unit)
  -> f: (string -> 'a)
  -> string -> string
  -> 'a

(**
   Runs [f] with a temporary dir as option and removes the directory afterwards.
*)
val with_temp_dir: ?in_dir:string -> string -> string -> f:(string -> 'a) -> 'a


val is_parent : string -> string -> bool
(**
   [is_parent dir1 dir2]
   returns [true] if [dir1] is a parent of [dir2]

   Note: This function is context independent, use [expand] if you want to
   consider relatives paths from a given point.
   In particular:
   - A directory is always the parent of itself.
   - The root is the parent of any directory
   - An absolute path is never the parent of relative one and vice versa.
   - ["../../a"] is never the parent of ["."] even if this could be true given
   form the current working directory.
*)
