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

val assert_true :
  ?msg:string -> bool -> unit

val assert_false :
  ?msg:string -> bool -> unit

val assert_none :
  ?msg:string -> 'a option -> unit

val assert_some :
  ?msg:string -> 'a option -> unit

val assert_equals :
  ?msg:string -> exp:'a -> 'a -> ('a -> string) -> unit

(* Asserts that the expected int matches the observed int. *)
val assert_int_equals :
  ?msg:String.t -> exp:int -> int -> unit

(* Asserts that the expected float matches the observed float. *)
val assert_float_equals :
  ?msg:String.t -> exp:float -> float -> unit

(* Asserts that the expected mfloat matches the observed mfloat. *)
(* val assert_mfloat_equals :
 *   ?msg:String.t -> exp:Mfloat.t -> Mfloat.t -> unit *)

(* Asserts that the expected string matches the observed string. *)
val assert_str_equals :
  ?msg:string -> exp:string -> string -> unit

(* Asserts that the specified function throws an exception. *)
val assert_throws_exception :
  ?msg:string -> (unit -> unit) -> unit

(* Forces the test to fail with the specified message. *)
val fail :
  string -> unit
