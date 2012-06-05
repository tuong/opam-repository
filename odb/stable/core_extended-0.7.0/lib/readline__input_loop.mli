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

(* This is an internal module: it shouldn't be used outside of core_extended*)
open Core.Std

type t

val create : ?text:string -> string list -> t
val print : prompt:string -> map_out:(string -> string) -> t -> unit
val contents : t -> string

type input = [ `Backspace
| `Char of char
| `Delete
| `Down
| `End
| `Eof
| `Home
| `Left
| `Right
| `Tab
| `Unknown_escape of (string*int option*int option)
| `Up ]


val step : ?completion:(left:string -> right:string -> string list) ->
  t -> input -> t
