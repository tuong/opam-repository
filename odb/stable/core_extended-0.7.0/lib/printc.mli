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


val (~:) : string list -> string         (** alias for String.concat *)
val (~%) : ('a,unit,string) format -> 'a (** alias for sprintf *)

(** printing functions *)

val print : string -> unit
val eprint : string -> unit
val fprint : out_channel -> string -> unit

(** printing functions that add endlines *)

val printl : string -> unit
val eprintl : string -> unit
val fprintl : out_channel -> string -> unit

(* adds padding to the left-hand side, filling with spaces by default *)
val lpad : ?fill:Char.t -> int -> string -> string

(* adds padding to the right-hand side, filling with spaces by default *)
val rpad : ?fill:Char.t -> int -> string -> string

(* [i2s n] returns a string representation of n *)
val i2s : int -> string

(* [f2s x] returns a string representation of x  *)
val f2s : float -> string
