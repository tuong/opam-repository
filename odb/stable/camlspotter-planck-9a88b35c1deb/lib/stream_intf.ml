(** Stream interface *)

open Spotlib.Spot
open Lazylist

(** Minimum specification to create a stream *)
module type Base = sig
  type elem
  (** Type of the stream element *)

  val show_elem : elem -> string
  (** [elem] must be printable *)

  val equal_elem : elem -> elem -> bool
  (** Element equality. 
      If you can specify something more efficient than the polymorphic equality (=) here,
      the entire parsing performance may be improved.
  *)

  type attr
  (** Type of the stream attribute *)

  val default_attr : attr
  (** Default attr value *)
    
  module Pos : Position.S
  (** Module for stream positions *)

  val position_of_attr : attr -> Pos.t
  (** [Pos.t] must be obtainable from [attr]. *)
end

(* Standard interface *)
module type S = sig
  include Base

  type t = (elem, attr) zlist
  (** Stream is implemented as a lazylist with attributes. *)

  val default_null : t
  (** Null with the default attr *)

  val null : attr -> t
  (** Create a null stream *)

  val null_desc : attr -> (elem, attr) desc
  val cons_desc : elem -> attr -> t -> (elem, attr) desc
  (** Create a null/cons cell desc. *)


  (** Destructors *)
  val desc : t -> (elem, attr) desc
  val peek : t -> (elem * attr * t) option

  val is_null : t -> bool
  (** Null check *)

  val attr : t -> attr
  (** Attribute of the head element/null *)

  val position : t -> Pos.t
  (** Position of the head element/null *)

  val to_list : t -> elem list
  val to_list_with_attrs : t -> (elem * attr) list
  (** Conversions to eager list. The attribute at Null is thrown away. *)

  val iter : (elem option -> attr -> unit) -> t -> unit
  val fold_right : (elem option -> attr -> 'a -> 'a) -> t -> 'a -> 'a
  val map : (elem -> attr -> 'a2 * 'attr2) -> (attr -> 'attr2) -> t -> ('a2, 'attr2) zlist
  (** Iteration, folding and map *)

  val rev_between : t -> t -> (elem * attr) list
  (** [rev_between t1 t2] returns the elemens between the two steam positions [t1] and [t2].
      [t2] must be a postfix of [t1].
      The elements are returned in the reverse order in the stream.

      NOTE:
      The behaviour of [rev_between t1 t2] when [t2] is not a postfix of [t1] is not defined.
      No sanity check at all.

      NOTE:
      This can be quite costy operation. Use with care.
  *)

  val between : t -> t -> (elem * attr) list
  (** Same as [rev_between] but the elements are in the same ordered in the streams.

      [between t1 t2] is actually defined as [List.rev (rev_between t1 t2)].
      The same notes of [rev_between] apply to this function too.
  *)
end
