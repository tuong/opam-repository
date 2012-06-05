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

(** Extensions to [Core.Core_array]. *)

val foldi : init:'a -> 'b array -> f:(int -> 'a -> 'b -> 'a) -> 'a


(** makes a random split & subset of an array; p (the fraction that you want to split) is
   constrained to be [0, 1].  Note that the length of the first array will be the closest
   integer to the fraction you desired, meaning that each element is NOT selected with
   probability exactly p. *)
val random_split : ?random_state:Random.State.t -> p:float -> 'a array -> ('a array * 'a array)
val random_sub : ?random_state:Random.State.t -> p:float -> 'a array  -> 'a array
