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

(** Extensions to [Core.Exn].*)

open Core.Std



(** The [to_string] function is slightly tweaked to avoid escaping the string
content of [Failure]. *)
val to_string : exn -> string

(** This is also an ever so slight variation of [to_string] target more at user
than developers ([Failure s] is just printed as [s])
*)
val to_string_hum : exn -> string

(** [unwrap e]

    Tries to unwrap an exception to find the original cause of the error
    (Finally for instance has the propency to burry exception...). This is
    useful when matching on exceptions.
*)
val unwrap : exn -> exn




(** The point of this module is to be able to include an exn in a type that has to be
    sexpable or binable.  The [Exn_string.t] type is more descriptive than just converting
    to a string and is guaranteed to have come from an exn (unless someone abuses the
    [t_of_sexp] function or something).
*)
module Exn_string : sig
  type t = private string
  include Sexpable with type sexpable = t
  include Stringable with type stringable = t
  include Binable with type binable = t

  val of_exn : exn -> t
end
