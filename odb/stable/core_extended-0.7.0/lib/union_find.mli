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
   Disjoint-sets.
*)


(** Union find is used to implement an equivalence relation on objects, where
    the equivalence relation can dynamically be coarsened by "union"ing two
    equivalence classes together.

    All of the operations are effectively (amortized) constant time.

    @see <en.wikipedia.org/wiki/Disjoint-set_data_structure> wikipedia.

    This implementation is in no way thread-safe.
*)

(** [type 'a t] is the type of objects, where each object is part of an
    equivalence class that is associated with a single value of type ['a]. *)
type 'a t

(** [create v] returns a new object in its own equivalence class that has value
    [v]. *)
val create : 'a -> 'a t

(** [get t] returns the value of the class of [t]. *)
val get : 'a t -> 'a

(** [set t v] sets the value of the class of [t] to [v]. *)
val set : 'a t -> 'a -> unit

(** [same_class t1 t2] returns true iff [t1] and [t2] are in the same equivalence
    class.
*)
val same_class : 'a t -> 'a t -> bool

(** [union t1 t2] makes the class of [t1] and the class of [t2] be the same
    (if they are already equal, then nothing changes).  The value of the
    combined class is the value of [t1] or [t2]; it is unspecified which.
    After [union t1 t2], it will always be the case that [equals t1 t2].
*)
val union: 'a t -> 'a t -> unit
