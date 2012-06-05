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

(**
   Atomically edit a file without long-term locking

   See documentation for the function [atomic_edit]
   *)
open Core.Std

(** the return value of [atomic_edit]
*)
type return_type =
  | Ok
  | Changed of string * string
  | Abort

(** [atomic_edit ~f filepath] edit [filepath] by making a unique copy (peer)
    that can be openly changed without holding any locks on the original. When
    the function returns, a short term exclusive lock is held while overwriting
    the original with the edited copy. If the mtime of the original is changed
    since the copy was made, the replacement is aborted (indicating another
    [atomic_edit] was first to update the file, or some other process) and
    Changed is returned with a tuple [(warning message * terd file)]. The terd
    file contains the edits and might be used to diff or re-edit.
*)
val atomic_edit : f:(string -> [ `Ok | `Abort ] ) -> string -> return_type
