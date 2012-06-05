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

(** Pervasive functions. *)


val run_main : (unit -> unit) -> _

(** [write_wrap ~atomic ~f fname] Runs [f] on an [out_channel]. If [mode] is
    [`Atomic] or [`Atomic_update] is set all the changes will be written to a
    temporary file which will then be moved over [fname] otherwise we are
    writing straight to [fname].

    Values for [mode]:
    - [`Clobber]: clear the file on opening (this is the default value)
    - [`Append]: append to the file
    - [`Atomic]: replace the file atomically when we are done writing it
    - [`Atomic_update]: replace the file atomically when we are done writing it
    iff its content has been modified.
*)
val write_wrap :
 ?mode:[`Clobber|`Append|`Atomic|`Atomic_update]
 -> f:(out_channel -> 'a)
 -> string
 -> 'a
