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

module Filesize = struct
  type t = Int64.t with bin_io

  module Infix64 = (Int64 :
    sig type t = Int64.t
        val (+) : t -> t -> t
        val (-) : t -> t -> t
        val ( * ) : t -> t -> t
        val (/) : t -> t -> t
    end)

  open Infix64


  let byte_dimension_suffix = ['b';'k';'m';'g';'t';'p';'z']

  let rec to_string__loop v order =
    let divd = v / 1024L in
    if divd * 1024L = v (* v mod 1024 = 0 *)
      && v >= 1024L then
        to_string__loop (v / 1024L) (Int.succ order )
    else
      let suffix = List.nth_exn byte_dimension_suffix order in
      Int64.to_string_hum v ^ (Char.to_string suffix)

  let to_string v = to_string__loop v 0

  let rec of_string__get_dimension suffix mult = function
    | [] -> failwithf "file_size_of_string: unknown size suffix %c" suffix ()
    | h::_ when h = suffix -> mult
    | _::t -> of_string__get_dimension suffix (1024L * mult) t

  let of_string s =
    let len = String.length s in
    if len = 0 then
      failwith "Int.filsize_of_string: empty string";
    match s.[pred len] with
    | '0'..'9' -> Int64.of_string s
    | c ->
        let main = String.sub s ~pos:0 ~len:(pred len) in
        let main = Int64.of_string main in
        let dim = of_string__get_dimension c 1L byte_dimension_suffix in
        main * dim

  let t_of_sexp = function
    | Sexp.List _ as sexp -> Sexplib.Conv.of_sexp_error
        "Extended_int.Filesize.t_of_sexp:expectibg an atom" sexp
    | Sexp.Atom a as sexp ->
        try
          of_string a
        with Failure msg ->
           Sexplib.Conv.of_sexp_error
            ("Extended_int.Filesize.t_of_sexp:" ^ msg) sexp

  let sexp_of_t i = Sexp.Atom (to_string i)

end

module Verified_spec = struct
  include Core.Std.Int64
  let module_name = "Int64"
end

include Number.Make_verified_std (Verified_spec)
