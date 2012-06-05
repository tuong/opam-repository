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

include Bin_prot.Binable
open Sexplib.Wrapper
open Bin_prot.Std

module Of_stringable (M : Stringable.S) =
  Bin_prot.Utils.Make_binable (struct
    module Binable = struct
      type t = string with bin_io
      type binable = t
    end
    type t = M.stringable
    let to_binable = M.to_string

    (* Wrap exception for improved diagnostics. *)
    exception Of_binable of string * exn with sexp
    let of_binable s =
      try
        M.of_string s
      with x ->
        raise (Of_binable (s, x))
  end)
