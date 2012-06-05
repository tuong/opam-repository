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
   Circular buffers.

   THIS LIBRARY WILL BE DELETED IN FAVOUR OF DEQUEUE AT SOME POINT IN THE FUTURE
*)



type 'a t = {
   	mutable data : 'a array; 	(*	base of circular buffer	*)
   	mutable start : int; 	(*	first position at which data is found	*)
   	mutable length : int; 	(*	number of elements in buffer	*)
   	never_shrink : bool; 	(*	whether to refrain from shrinking the buffer	*)
   	dummy : 'a; 	(*	value used to pack into newly allocated arrays	*)
} with sexp

val create : ?never_shrink:bool -> 'a -> Core.Std.Int.comparable -> 'a t

val length : 'a t -> int

val phys_length : 'a t -> int

val is_full : 'a t -> bool

val check_index : string -> 'a t -> int -> unit

val get : 'a t -> int -> 'a

val set : 'a t -> int -> 'a -> unit

(** copies data from one array to another.  This assumes that the destination
    array is large enough to accommodate the data *)
val copy_data : src:'a Core.Std.Array.container ->
       dst:'a Core.Std.Array.t -> int -> int -> unit

(** [swap_array buf len] copies the contents of [buf] to a new array of length
    [len] and places that new data into the buffer *)
val swap_array : 'a t -> int -> unit

(**
   double the size of the buffer
*)
val expand : 'a t -> unit


(**
   half the size of the buffer
*)
val shrink : 'a t -> unit


val to_array : 'a t -> 'a Core.Std.Array.t

val add : 'a t -> 'a -> unit

val drop_from : 'a t -> int -> unit

val drop_last : 'a t -> unit

(** [iter buf ~f] calls func on each buffer element starting with 0
    like this: (func pos element) *)
val iter : 'a t -> f:(int -> 'a -> 'b) -> unit

(** [iterr buf ~f] calls func on each buffer element starting with end
    like this: (func pos element) *)
val iterr : 'a t -> f:(int -> 'a -> 'b) -> unit

(** initialize Cbuffer from array *)
val of_array : 'a Core.Std.Array.container -> 'a t

(** compare two buffer fragments *)
val cb_compare : f:('a -> 'b -> bool) ->
       b1:'a t -> b2:'b t -> s1:int -> s2:int -> n:int -> bool

(** drop stuff from the end.
    equivalent to
    while cutoff > f (get buf (length buf - 1)) do drop_last buf; done
    but calls drop just once *)
val drop_old : ?cmp:('a -> 'a -> int) ->
       ?free:('b -> unit) -> f:('b -> 'a) -> cutoff:'a -> 'b t -> int

