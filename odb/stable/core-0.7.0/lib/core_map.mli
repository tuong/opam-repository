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

(** Polymorphic map module.  Just like the standard Map module, but with the
    order of the two type parameters inverted (we think [key,value] makes a lot
    more sense than [value,key]).  There are also a few additional Map
    operations in here.  If you open Core.Std, this is your Map module. *)

(** {6 Polymorphic versions of standard Map operations} *)

open Core_map_intf

(* S2 is a polymorphic map *)
include S2

module type Key = Key
module type S = S

module type S_binable = sig
  include S
  include Binable.S1 with type 'a binable = 'a t
end

module Make (Key : Key) : S with type key = Key.t

module Make_binable (Key : sig
  include Key
  include Binable.S with type binable = t
end) : sig
  include S_binable
    with type key = Key.t
    with type 'a t = 'a Make(Key).t
end
