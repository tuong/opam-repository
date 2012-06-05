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

type ('a, 'b) t =
  | Ok of 'a
  | Error of 'b
with sexp, bin_io

type ('a, 'b) _t = ('a, 'b) t

include Monad.Make2
(struct
   type ('a, 'b) t = ('a,'b) _t

   let bind x f = match x with
     | Error _ as x -> x
     | Ok x -> f x

   let return x = Ok x
 end)

type ('a, 'b) sexpable = ('a, 'b) t
type ('a, 'b) binable = ('a, 'b) t

let fail x = Error x;;
let failf format = Printf.ksprintf fail format

(* This definition shadows the version created by the functor application above, but it
   is much more efficient. *)
let map t ~f = match t with
  | Ok x -> Ok (f x)
  | Error _ as x -> x

let map_error t ~f = match t with
  | Ok _ as x -> x
  | Error x -> Error (f x)

let is_ok = function
  | Ok _ -> true
  | Error _ -> false

let is_error = function
  | Ok _ -> false
  | Error _ -> true

let ok = function
  | Ok x -> Some x
  | Error _ -> None

let error = function
  | Ok _ -> None
  | Error x -> Some x

let of_option opt ~error =
  match opt with
  | Some x -> Ok x
  | None -> Error error

let iter v ~f = match v with
  | Ok x -> f x
  | Error _ -> ()

let call ~f x =
  match f with
  | Ok g -> g x
  | Error _ -> ()

let apply ~f x =
  match f with
  | Ok g -> Ok (g x)
  | Error _ as z -> z

let ok_fst = function
  | Ok x -> `Fst x
  | Error x -> `Snd x

let ok_if_true bool ~error =
  if bool
  then Ok ()
  else Error error

let try_with f =
  try Ok (f ())
  with exn -> Error exn

let ok_exn ?fail = function
  | Ok x -> x
  | Error _ ->
    match fail with
    | None -> failwith "Result.ok_exn"
    | Some exn -> raise exn
;;

let raise_error = function
  | Ok x -> x
  | Error exn -> raise exn
;;

module Export = struct
  type ('ok, 'err) _result =
    ('ok, 'err) t =
      | Ok of 'ok
      | Error of 'err
end
