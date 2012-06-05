(******************************************************************************
 *                             Core                                           *
 *                                                                            *
 * Copyright (C) 2008- Jane Street Holding, LLC                               *
 *    Contact: opensource@janestreet.com                                      *
 *    WWW: http://www.janestreet.com/ocaml                                    *
 *                                                                            *
 *                                                                            *
 * This file is derived from source code of the Ocaml compiler.               *
 * which has additional copyrights:                                           *
 *                                                                            *
 *    Xavier Leroy, projet Cristal, INRIA Rocquencourt                        *
 *                                                                            *
 *    Copyright 1999 Institut National de Recherche en Informatique et        *
 *    en Automatique.                                                         *
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

(** Polymorphic set module.  Just like the standard Set module, but
    with some extra Set operations.  If you open Core.Std, this is your
    Set module. *)

(** {6 Polymorphic versions of standard Set operations} *)

open Core_set_intf

(* S1 is a polymorphic set *)
include S1

module type Elt = Elt
module type S = S

module type S_binable = sig
  include S
  include Binable.S with type binable = t
end

module Make (Elt : Elt) : S with type elt = Elt.t

module Make_binable (Elt : sig
  include Elt
  include Binable.S with type binable = t
end) : S_binable with type elt = Elt.t
