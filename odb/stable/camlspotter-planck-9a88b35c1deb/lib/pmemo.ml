open Spotlib.Spot
open Planck_intf
open Result

module Extend(Str : sig 
  include Stream_intf.S
  type key
  val memoize : key -> (t -> 'a) -> t -> 'a
end )(Base : Planck_intf.S 
      with type Str.elem = Str.elem
      and  type Str.attr = Str.attr
      and  type Str.Pos.t = Str.Pos.t) = struct

  let memoize = Str.memoize
end
