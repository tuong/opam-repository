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

(** Simple boolean language *)

(** Blang provides infrastructure for writing simple boolean DSLs.
    All expressions in a Blang language evaluate to a bool.  To get a
    concrete language, you need to parameterize the language with some
    value type, where values can also be evaluated to true or false.

   The syntax is almost exactly the obvious s-expression syntax,
   except that:

   1. Base elements do not need to be marked explicitly.  Thus, if your
      value language had two elements, "true" and "false", then you
      could write the following Blang s-expressions:

        true
        (if true true false)

      and so on.  Note that this can get in the way of using the blang
      keywords in your value language.

   2. the argument lists for And and Or should not be wrapped in parens.
      i.e., you can write:

        (and true (or true false false) (and true) (and) (not (or)))
*)

open Std_internal


type 'a t =
  | And of 'a t list
  | Or of 'a t list
  | Not of 'a t
  | If of 'a t * 'a t * 'a t
  | Base of 'a

(** Note that the sexps are not directly inferred from the type above --
    there are lots of fancy shortcuts.  Also, the sexps for ['a] must not
    look anything like blang sexps.  Otherwise [t_of_sexp] will fail. *)
include Sexpable.S1 with type 'a sexpable = 'a t
include Binable.S1 with type 'a binable = 'a t

(** [values t] forms the list containing every [v]
    for which [Base v] is a subexpression of [t] *)
val values : 'a t -> 'a list

val true_ : 'a t
val false_ : 'a t

(** [Blang.t]'s monad works as follows:
    {ul
      {- [return v] is the term [Base v].}
      {- [bind t k] is a substitution operation that replaces every
         [Base v] in [t] with the term [k v].}
    }
*)
include Monad with type 'a monad = 'a t

val eval : ('a -> bool) -> 'a t -> bool
