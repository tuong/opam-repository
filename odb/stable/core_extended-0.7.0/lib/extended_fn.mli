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


(** an abstract type of functions that are returned as the result of
    partially applying some other function *)
module Abstract : sig
  type +'a t
  
  val fn : ('a -> 'b) -> ('a -> 'b) t
  val app : ('a -> 'b) t -> 'a -> 'b
end

(* example usage:

  open Core_extended.Std

  module M : sig
    val add1 : int -> int -> int -> int -> int
    val add2 : int -> int -> (int -> int -> int) Fn.Abstract.t
      (* [add2] is intended to be partially applied *)
  end = struct
    let add1 a b c d = a + b + c + d
    let add2 a b = Fn.Abstract.fn (fun c d -> a + b + c + d)
  end

  let x = M.add1 1 2 3 4 in
  ..

  (* the type of M.add2 guarantees that it is paritally applied *)
  let f = Fn.Result.app (M.add2 1 2) in
  let x = f 3 4 in
  ...

  (* Note that the previous usage is more efficient than the following *)
  let f = M.add2 1 2 in
  let x = Fn.Result.app f 3 4 in
  ...

*)


