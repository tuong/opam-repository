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

module T = struct
  type 'a t =
  | And of 'a t list
  | Or of 'a t list
  | Not of 'a t
  | If of 'a t * 'a t * 'a t
  | Base of 'a
  with bin_io
end
include T
type 'a sexpable = 'a t
type 'a binable = 'a t

let eval sub_eval expr =
  let rec eval = function
    | Base x -> sub_eval x
    | And l -> List.for_all l ~f:eval
    | Or l -> List.exists l ~f:eval
    | Not t -> not (eval t)
    | If (t1,t2,t3) -> if eval t1 then eval t2 else eval t3
  in
  eval expr
;;

(* Simple flattening s-expression parser and printer *)

type sexp = Sexp.t = Atom of string | List of sexp list (* cheap import *)

let unary name list sexp =
  match list with
  | [x] -> x
  | _ ->
    of_sexp_error
      (sprintf "%s expects one argument, %d found"
         name (List.length list))
      sexp

let trinary name list sexp =
  match list with
  | [x; y; z] -> (x, y, z)
  | _ ->
    of_sexp_error
      (sprintf "%s expects three arguments, %d found"
         name (List.length list))
      sexp

let sexp_of_t sexp_of_value t =
  let rec sexp_of_t t =
    match t with
    | Base x -> sexp_of_value x
    | And x -> List (Atom "and" :: List.map ~f:sexp_of_t x)
    | Or x -> List (Atom "or" :: List.map ~f:sexp_of_t x)
    | Not t -> List [Atom "not"; sexp_of_t t]
    | If (t1, t2, t3) ->
        List [Atom "if"; sexp_of_t t1; sexp_of_t t2; sexp_of_t t3]
  in
  sexp_of_t t
;;

let rec t_of_sexp value_of_sexp sexp =
  let value sexp = Base (value_of_sexp sexp) in
  let rec of_sexp sexp =
    match sexp with
    | List (Atom kw :: args) ->
      begin match String.lowercase kw with
      | "and" -> And (List.map ~f:of_sexp args)
      | "or" -> Or (List.map ~f:of_sexp args)
      | "not" -> Not (of_sexp (unary "not" args sexp))
      | "if" ->
        let (x,y,z) = trinary "if" args sexp in
        If (of_sexp x,of_sexp y,of_sexp z)
      | _ -> value sexp
      end
    | _ -> value sexp
  in
  of_sexp sexp
;;

let values t =
  let rec collect_values vs = function
    | [] -> vs
    | (Base v)::ts -> collect_values (v::vs) ts
    | (Not t)::ts -> collect_values vs (t::ts)
    | (If (t1, t2, t3))::ts -> collect_values vs (t1::t2::t3::ts)
    | (And ts | Or ts)::ts' -> collect_values vs (ts @ ts')
  in
  List.rev (collect_values [] [t])
;;


let true_ = And []
let false_ = Or []

include Monad.Make (struct
  type 'a t = 'a T.t
  let rec bind t k = match t with
    | And ts -> And (List.map ts ~f:(fun t -> bind t k))
    | Or ts -> Or (List.map ts ~f:(fun t -> bind t k))
    | Not t -> Not (bind t k)
    | If (t1, t2, t3) -> If (bind t1 k, bind t2 k, bind t3 k)
    | Base v -> k v
  ;;
  let return v = Base v
end)
