(** Stream module. See Stream_intf for more details *)
open Stream_intf

module Make (Base : Base) : S 
  with module Pos = Base.Pos
  and  type elem  = Base.elem
  and  type attr  = Base.attr
(** The functor [Make] creates a stream module based on [Base] *)

