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

(** Various system utility functions. *)

(** Get the default editor (program) for this user.
    This functions checks the EDITOR and VISUAL environment variables and then
    looks into a hard coded list of editors.
*)
val get_editor : unit -> string option

(**
   [checked_edit ~check file]

   Edit a file in safe way, like [vipw(8)]. Launches your default editor on
   [file] and uses [check] to check its content.

   @param check a function returning a text representing the error in the file.
   @param create create the file if it doesn't exists. Default [true]
   @return [`Ok] or [`Abort]. If [`Abort] is returned the files was not modified
   (or created).
*)

val checked_edit :
  ?create:bool
  
  -> check:(string -> string option)
  -> string
  
  -> [ `Abort | `Ok ]


(** Edit files containing sexps. *)
module Sexp_checked_edit (S:Sexpable): sig
  val check : string -> string option

  val edit :
    ?create:bool
    -> string
    -> [ `Abort | `Ok ]

end
