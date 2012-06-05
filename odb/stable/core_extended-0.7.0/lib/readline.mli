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

(**
   Interactive line editing.

   This implements very basic [readline] abilities: backspace, left and right
   arrows work as expected.
   There's also a history that can be browsed through the [up] and [down] arrows.
*)


type completer = (left:string -> right:string -> string list)

module History : sig
  type t

  val null : t
    (** A value which is always empty *)

  val default : t
  val create : int -> t
  val flush : t -> unit
  val to_list : t -> string list
  val of_list : ?size:int -> string list -> t
end
(**
   A mutable variable representing the history.
*)

val input_line :
  ?history:History.t ->
  ?prompt:string ->
  ?text:string ->
  ?tab_completion:completer -> unit -> string option
(**
   @param prompt the string to use as a prompt (default value [">"])
   @param history the mutable value used as a history. The deault value is
   [History.default]. If you don't want any history ou should use [History.null]
   @param tab_completion the function use to complete on tab. By default there
   is no completion.

   @return [None] on [EOF] (i.e. the user typed [ctrl + d])
*)

val input_line_eof :
  ?history:History.t ->
  ?prompt:string ->
  ?text:string ->
  ?tab_completion:completer -> unit -> string


val password : ?prompt:string -> unit -> string option
  (** Prompts for a password. Displays '*' instead of typed characters.
      @return [None] on [EOF]
  *)

val confirm : ?prompt:string -> string -> bool
(** Prompts for an answer. Returns true if the answer equals the given string
    (ignoring case), false otherwise. *)

val choice : (string * 'a) list -> 'a option
(** [ choice ["a",a;"b",b] ]
    Prompts the user to choose a between several value (associated to strings)
    and returns the value chosen by the user.
    @return [None] on [EOF].
*)
