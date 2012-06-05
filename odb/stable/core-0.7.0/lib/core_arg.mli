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

type spec = Caml.Arg.spec =
| Unit of (unit -> unit)      (* Call the function with unit argument *)
| Bool of (bool -> unit)      (* Call the function with a bool argument *)
| Set of bool ref             (* Set the reference to true *)
| Clear of bool ref           (* Set the reference to false *)
| String of (string -> unit)  (* Call the function with a string argument *)
| Set_string of string ref    (* Set the reference to the string argument *)
| Int of (int -> unit)        (* Call the function with an int argument *)
| Set_int of int ref          (* Set the reference to the int argument *)
| Float of (float -> unit)    (* Call the function with a float argument *)
| Set_float of float ref      (* Set the reference to the float argument *)
| Tuple of spec list          (* Take several arguments according to the spec list *)
| Symbol of string list * (string -> unit) (* Take one of the symbols as argument and call the function with the symbol *)
| Rest of (string -> unit) (* Stop interpreting keywords and call the function with each remaining argument *)

type key = string 

type doc = string

type t = key * spec * doc

type usage_msg = string 

type anon_fun = string -> unit 

val parse : t list -> anon_fun -> usage_msg -> unit

val parse_argv :
  ?current:int ref
  -> string array
  -> t list -> anon_fun -> usage_msg -> unit

exception Help of string

exception Bad of string

val usage : t list -> usage_msg -> unit

val align : t list -> t list

val sort_and_align : t list -> t list

val current : int ref
