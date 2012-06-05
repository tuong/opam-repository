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


(** An array that can shrink and expand on both ends - the minimum index need not be 0.
    can easily be used as a double-ended queue
    unlike cbuffer, an index refers to the same element after Dequeue.push_front
    the "front" is the smallest valid index, while the "back" is the largest
    all operations are amortized O(1) with a small constant *)

open Std_internal

type 'a t

include Sexpable.S1 with type 'a sexpable = 'a t

(* if never_shrink is true, the physical array will never shrink; only expand *)
(* initial_index is the index at which the first push_back operation will insert *)
(* a dummy element is required to satisfy the type-checker and will never be returned *)

val create : ?never_shrink:bool -> ?initial_index:int -> dummy:'a -> unit -> 'a t

(* number of elements in the dequeue, i.e. back_index - front_index + 1 *)
val length : 'a t -> int
(* same as Dequeue.length = 0 *)
val is_empty : 'a t -> bool

(* minimum and maximum valid indices (inclusive) *)
val front_index : 'a t -> int
val back_index : 'a t -> int

(* returns an element, and leaves it in the dequeue *)
(* [get q i] raises Invalid_argument unless front_index <= i <= back_index *)
val get : 'a t -> int -> 'a

(* raises Invalid_argument iff dequeue is empty *)

val get_front : 'a t -> 'a
val get_back : 'a t -> 'a

(* mutates the indexed element *)
val set : 'a t -> int -> 'a -> unit

(* same as Array.iteri (iterates passing the index) *)
val iteri : f:(int -> 'a -> unit) -> 'a t -> unit

(* same as iteri but don't pass the index *)
val iter : f:('a -> unit) -> 'a t -> unit

(* fold across the index element pairs of the dequeue *)
val foldi : f:('a -> int -> 'b -> 'a) -> init:'a -> 'b t -> 'a

(* fold across just the elements of the dequeue *)
val fold : f:('a -> 'b -> 'a) -> init:'a -> 'b t -> 'a

(* decreases front_index by one, and places the new element at the new front_index *)
val push_front : 'a t -> 'a -> unit

(* increases back_index by one, and places the new element at the new back_index *)
val push_back : 'a t -> 'a -> unit

(* drop functions raise Invalid_argument if asked to drop more than Dequeue.length
   elements *)

(* drops n elements (default 1) at front *)
val drop_front : ?n:int -> 'a t -> unit
(* drops n elements (default 1) at back *)
val drop_back : ?n:int -> 'a t -> unit

(* drop the front and return it *)
val take_front : 'a t -> 'a

(* drop the back and return it *)
val take_back : 'a t -> 'a

(* drops index j iff j < i *)
val drop_indices_less_than : 'a t -> int -> unit
(* drops index j iff j > i *)
val drop_indices_greater_than : 'a t -> int -> unit

val invariant : 'a t -> unit
