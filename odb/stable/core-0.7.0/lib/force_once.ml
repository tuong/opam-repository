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

type 'a z = 
| Forced
| Not_forced of (unit -> 'a)

type 'a t = 'a z ref

let create f = ref (Not_forced f)

let ignore () = create (fun () -> ())

let force t =
  match !t with
  | Forced -> failwith "Force_once.force"
  | Not_forced f -> t := Forced; f()
      
let sexp_of_t _ t =
  match !t with
  | Forced -> Sexp.Atom "<Forced>"
  | Not_forced _ -> Sexp.Atom "<Not_forced>"
