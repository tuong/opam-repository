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

type 'a node =
  | Unevaluated of (unit -> 'a)
  | Evaluating
  | Evaluated_to_val of 'a
  | Evaluated_to_exn of exn

type 'a t = 'a node ref

type 'a lazy_m = 'a t

let of_val v = ref (Evaluated_to_val v)

let of_fun f = ref (Unevaluated f)

exception Undefined

let force t =
  match !t with
  | Evaluated_to_val v -> v
  | Evaluated_to_exn e -> raise e
  | Unevaluated f ->
      begin
        t := Evaluating;
        match (try `Val (f ()) with e -> `Exn e) with
        | `Val v -> (t := Evaluated_to_val v; v)
        | `Exn e -> (t := Evaluated_to_exn e; raise e)
      end
  | Evaluating -> raise Undefined
;;

let map m ~f = of_fun (fun () -> f (force m))

let is_val t =
  match !t with
  | Evaluated_to_val _ -> true
  | Unevaluated _
  | Evaluating
  | Evaluated_to_exn _ -> false
;;

include Monad.Make(struct
  type 'a t = 'a lazy_m
  let return x = of_val x
  let bind m f = of_fun (fun () -> force (f (force m)))
  let failwith str = Pervasives.failwith str
end);;

