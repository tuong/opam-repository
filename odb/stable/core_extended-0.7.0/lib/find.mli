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

(** Unix like [find].*)

(* Implements find (like the unix utility).  Note that t is stateful both because
  filesystems themselves are highly stateful, and for performance reasons *)


open Core.Std

type t
type file_info = string * Unix.stats

module Options : sig
  type error_handler =
      | Ignore
      | Print
      | Raise
      | Handle_with of (string -> unit)

  type t = {
    max_depth: int option;
    follow_links: bool;
    on_open_errors: error_handler;
    on_stat_errors: error_handler;
    filter: (file_info -> bool) option;
    skip_dir : (file_info -> bool) option;
  }

  val default : t
  val ignore_errors : t
end

(** [create ?options dir] create a Find.t based in dir *)
val create : ?options:Options.t -> string -> t

(** [next t] return the next file from the collection of valid files in t or None
  if no more files remain *)
val next : t -> file_info option

(** [close t] drops all the resources associated with t.  It is a mistake to attempt to
  use t again.  Any Find.t will be automatically closed after the last file is read by
  any means. *)
val close : t -> unit

(** [iter t ~f] calls f on every file in t *)
val iter : t -> f:(file_info -> unit) -> unit

(** [fold t ~init ~f] folds f over the files in t *)
val fold : t -> init:'a -> f:('a -> file_info -> 'a) -> 'a

(** [to_list t] returns all of the remaining files in t as a list in the order they
  would have been returned by subsequent calls to next *)
val to_list : t -> file_info list

(** [find_all ?options dir] short for to_list (create ?options dir) *)
val find_all : ?options:Options.t -> string -> file_info list
