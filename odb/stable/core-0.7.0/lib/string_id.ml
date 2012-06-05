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

include String

let check s =
  let stripped = String.strip s in
  if not (String.(=) stripped s) then
    Some (sprintf ("'%s' is not a valid identifier " ^^
                      "because it has whitespace on the edge")
             s)
  else if String.(=) s "" then
    Some "Attempt to use empty identifier"
  else if String.contains s '|' then
    Some "Identifier contains a pipe '|'"
  else
    None

let of_string s =
  match check s with
  | None -> s
  | Some err -> invalid_arg err

let t_of_sexp sexp =
  let s = String.t_of_sexp sexp in
  match check s with
  | None -> s
  | Some err -> of_sexp_error err sexp
