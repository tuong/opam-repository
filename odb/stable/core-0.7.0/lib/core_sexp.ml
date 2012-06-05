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

module Sexp = Sexplib.Sexp
open Sexplib.Wrapper
open Bin_prot.Std

include Sexp

type sexpable = t
type stringable = t

include (struct
  type t = Sexp.t = Atom of string | List of t list with bin_io
  type binable = t
  type sexpable = t
end : Interfaces.Binable with type binable = t)

module Sexp_option = struct
  type 'a sexp_option = 'a option with bin_io
end

module Sexp_list = struct
  type 'a sexp_list = 'a list with bin_io
end

module Sexp_array = struct
  type 'a sexp_array = 'a array with bin_io
end

module Sexp_opaque = struct
  type 'a sexp_opaque = 'a with bin_io
end

module Sexp_maybe = struct

  type sexp = t with bin_io             (* avoid recursive type *)
  type 'a t = ('a, sexp) Result.t with bin_io

  type 'a binable  = 'a t
  type 'a sexpable = 'a t

  let sexp_of_t sexp_of_a t =
    match t with
    | Result.Ok a -> sexp_of_a a
    | Result.Error sexp -> sexp

  let t_of_sexp a_of_sexp sexp =
    try Result.Ok (a_of_sexp sexp)
    with exn -> Result.Error (Exn.sexp_of_t exn)

end

let of_int_style = Int_conversions.sexp_of_int_style
