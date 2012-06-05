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

(**
   Extensions to [Core.Core_String] .
*)

(**
   [collate s1 s2] sorts string in an order that's is usaully more suited
   for human consumption by treating ints specificaly:
   (e.g. it will output: [["rfc1.txt";"rfc822.txt";"rfc2086.txt"]]).

   It works by splitting the strings in numerical and non numerical chunks and
   comparing chunks two by two from left to right (and starting on a non
   numerical chunks):
   - Non_numerical chunks are compared using lexicographical ordering.
   - Numerical chunks are compared based on the values of the represented ints
   and the number of trailing zeros.

   It is a total order.
*)
val collate : string -> string -> int

(**
   [unescaped s] is the inverse operation of [escaped]: it takes a string where
   all the special characters are escaped following the lexical convention of
   OCaml and returns an unescaped copy.
   The [strict] switch is on by default and makes the function treat illegal
   backslashes as errors.
   When [strict] is [false] every illegal backslash except escaped numeral
   greater than [255] is copied literally. The aforementioned numerals still
   raise errors. This mimics the behaviour of the ocaml lexer.
*)
val unescaped : ?strict:bool -> string -> string

(**
   Same as [unescaped] but instead of raising [Failure _] returns an error
   message with the position in the string in case of failure.
*)
val unescaped_res : ?strict:bool -> string -> (string,(int*string)) Core.Result.t

(** [squeeze str] reduces all sequences of spaces, newlines, tables, and
 * carriage returns to single spaces.
 *)

val squeeze : string -> string

(** [is_substring ~substring t] returns [true] if substring is a substring
 * of t.
 *)
val is_substring : substring:string -> string -> bool

(** [pad_left ~char s len]
    Returns [s] padded to the length [len] by adding characters [char] to the
    left of the string. If s is already longer than [len] it is returned unchanged.
*)
val pad_left : ?char:char -> string -> int -> string
val pad_right : ?char:char -> string -> int -> string

(**deprecated in favour of word_wrap *)
val line_break: len:int -> string -> string list

(**
   [word_wrap ~soft_limit s]

   Wraps the string so that it fits the length [soft_limit]. It doesn't break
   words unless we go over [hard_limit].

   if [nl] is passed it is inserted instead of the normal newline character.
*)
val word_wrap:
  ?trailing_nl:bool
  -> ?soft_limit:int
  -> ?hard_limit:int
  -> ?nl:string
  -> string
  -> string

module Escaping : sig
  (**
     String escaping.

     Operations for escaping and unescaping strings, with paramaterized escape
     and escapeworthy characters.
  *)
  

  
  (** [escape_gen escapeworthy_map escape_char s] returns an escaped string based on
      [s] as follows: if [(c1,c2)] is in [escapeworthy_map], then all occurences of
      [c1] are replaced by [escape_char] concatenated to [c2]. *)
  val escape_gen :
    escapeworthy_map:(char * char) list -> escape_char:char -> string -> string

  (** [escape escapeworthy escape_char s] is
      [escape_gen ~escapeworthy_map:(List.combine_exn escapeworthy escapeworthy)
      ~escape_char]. *)
  val escape : escapeworthy:char list -> escape_char:char -> string -> string

  (** [escape_one_orig ~escapeworthy ~escape_char s] escapes character
      [escapeworthy] with [escape_char] in string [s].  The function
      returns the original string if no character had to be escaped. *)
  
  val escape_one_orig :
    escapeworthy : char -> escape_char : char -> string -> string

  (** [escape_two_orig ~escapeworthy1 ~escapeworthy2 ~escape_char s]
      escapes characters [escapeworthy1] and [escapeworthy2] with
      [escape_char] in string [s].  The function returns the original
      string if no character had to be escaped. *)
  val escape_two_orig :
    escapeworthy1 : char -> escapeworthy2 : char -> escape_char : char -> string
    -> string

  (** [unescape_gen] is the inverse operation of [escape_gen], assuming an inverse
      map is given.  That is, [unescape_gen map escape_char s] returns an escaped string
      based on [s] as follows: if [(c1,c2)] is in [map], then all occurrences of
      [escape_char][c1] are replaced by [c2]. *)
  val unescape_gen :
    map:(char * char) list -> escape_char:char -> string -> string

  (** [unescape escape_char s] is [unescape_gen ~map:\[\] ~escape_char str] *)
  val unescape : escape_char:char -> string -> string
end
