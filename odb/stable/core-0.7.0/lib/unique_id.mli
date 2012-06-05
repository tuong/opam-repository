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

open Std_internal

(* An abstract unique identifier based on ordinary OCaml integers.  Be careful,
   this may easily overflow on 32bit platforms!  Int63 is a safer choice for
   portability.

   [Int] is useful when one is passing unique ids to C and needs a guarantee
   as to their representaion.  [Int] is always represented as an integer, while
   [Int63] is either an integer (on 64-bit machines) or a pointer (on 32-bit
   machines).

   If you do the following:

     module Id1 = Int (Unit)
     module Id2 = Int (Unit)

   then the types Id1.t and Id2.t are equivalent.  On the other hand, if you do

     module Id1 : Id = Int (Unit)
     module Id2 : Id = Int (Unit)

   then the types Id1.t and Id2.t are distinct.  Thus, you should use the latter
   form.
*)
module Int (Z : Unit) :
  Unique_id_intf.Id with type t = private int

(* An abstract unique identifier based on 63 bit integers. *)
module Int63 (Z : Unit) :
  Unique_id_intf.Id with type t = private Core_int63.t
