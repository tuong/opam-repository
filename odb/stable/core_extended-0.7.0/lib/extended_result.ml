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
module Ok = Result

module Error =
  Monad.Make2
    (struct
      type ('a, 'b) t = ('b,'a) Result.t

      let bind x f = match x with
        | Error x -> f x
        | Ok _ as x -> x

      let return x = Error x
    end)

module Exn = struct
  module T = struct
    type 'a t = ('a, exn) Result.t with sexp_of
  end
  include T

  include Monad.Make (struct
    include T

    let return x = Ok x

    let bind (t : 'a t) f =
      match t with
      | Ok x -> f x
      | Error e -> Error e
    ;;
  end)

  let ok = function
    | Ok a -> a
    | Error exn -> raise exn
  ;;
end
