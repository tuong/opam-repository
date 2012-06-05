(** Parser module extension for memoization over stream *)

(** Extend(Str)(Base) extends Base for Str with memoization
    Typical usage is:

    module MyParser = struct
      module Base = ...(Str)... (* creation of Base *)
      include Base
      include Pmemo.Extend(Str)(Base)
    end
*)

module Extend(Str : sig 
  include Stream_intf.S
  type key
  val memoize : key -> (t -> 'a) -> t -> 'a
end )(Base : Planck_intf.S 
      with type Str.elem = Str.elem
      and  type Str.attr = Str.attr
      and  type Str.Pos.t = Str.Pos.t) : sig 
  val memoize : Str.key -> (Str.t -> 'a) -> Str.t -> 'a 
end
