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
  (** Extension to the {Core.Result} *)

module Ok: Monad.S2 with type ('a,'err) monad = ('a,'err) Result.t
module Error: Monad.S2 with type ('err,'a) monad = ('a,'err) Result.t

module Exn : sig
  type 'a t = ('a, exn) Result.t

  val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t

  include Monad.S with type 'a monad = 'a t

  (** [ok t] returns [x] if [t = Ok x], or raises [e] if [t = Error e]. *)
  val ok : 'a t -> 'a
end
