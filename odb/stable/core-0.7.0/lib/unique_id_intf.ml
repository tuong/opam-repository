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

open Std_internal

module type Id = sig
  type t

  (* CAVEAT: Values created with of_float, of_sexp, or of_string may be equal to
     previously created values. *)
  include Comparable with type comparable = t
  include Hashable with type hashable = t

  include Intable with type intable = t
  (* The sexps and strings look like integers. *)
  include Sexpable with type sexpable = t
  include Binable with type binable = t
  include Stringable with type stringable = t

  (* Always returns a value that is not equal to any other value created with [create]. *)
  val create : unit -> t
end
