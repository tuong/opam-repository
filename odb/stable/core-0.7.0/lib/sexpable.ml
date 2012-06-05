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

open Sexplib

let failwithf = Core_printf.failwithf



module type S = sig
  type sexpable
  val sexp_of_t : sexpable -> Sexp.t
  val t_of_sexp : Sexp.t -> sexpable
end

module type S1 = sig
  type 'a sexpable
  val sexp_of_t : ('a -> Sexp.t) -> 'a sexpable -> Sexp.t
  val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a sexpable
end

module type S2 = sig
  type ('a, 'b) sexpable
  val sexp_of_t :
    ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b) sexpable -> Sexp.t
  val t_of_sexp :
    (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) sexpable
end

module type S3 = sig
  type ('a, 'b, 'c) sexpable
  val sexp_of_t :
    ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t)
    -> ('a, 'b, 'c) sexpable -> Sexp.t
  val t_of_sexp :
    (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c)
    -> Sexp.t -> ('a, 'b, 'c) sexpable
end

module Of_stringable (M : Stringable.S)
  : S with type sexpable = M.stringable = struct
  type sexpable = M.stringable
  let t_of_sexp sexp =
    match sexp with
    | Sexp.Atom s -> M.of_string s
    | Sexp.List _ -> Conv.of_sexp_error "t_of_sexp" sexp
  let sexp_of_t t = Sexp.Atom (M.to_string t)
end

module To_stringable (M : S) : Stringable.S with type stringable = M.sexpable =
struct
  type stringable = M.sexpable
  let of_string x = Conv.of_string__of__of_sexp M.t_of_sexp x
  let to_string x = Conv.string_of__of__sexp_of M.sexp_of_t x
end
