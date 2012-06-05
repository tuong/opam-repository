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

exception Not_a_record_type of Sexp.t with sexp

let fields sexp =
  match sexp with
  | Sexp.Atom _ -> raise (Not_a_record_type sexp)
  | Sexp.List l ->
    List.map l ~f:(function
      | Sexp.List (Sexp.Atom field::_) -> field
      | _ -> raise (Not_a_record_type sexp)
    )

module Make (M:Sexpable) = struct
  type t = {
    value : M.sexpable;
    extra_fields : string list
  }
  type sexpable = t

  let sexp_of_t t = M.sexp_of_t t.value

  let t_of_sexp sexp =
    let ef_ref = Sexplib.Conv.record_check_extra_fields in
    let prev_ef = !ef_ref in
    ef_ref := false;
    let value = M.t_of_sexp sexp in
    ef_ref := prev_ef;
    let used_fields = fields (M.sexp_of_t value) in
    let all_fields = fields sexp in
    let extra_fields = List.filter all_fields ~f:(fun field ->
      not (List.mem field ~set:used_fields)
    )
    in
    {
      value = value;
      extra_fields = extra_fields;
    }
end
