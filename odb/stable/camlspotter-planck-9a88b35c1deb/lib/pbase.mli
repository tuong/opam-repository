(** The basic monadic parser module. See Planck_intf.ml for more details *)

module Make( S : Stream_intf.S ) : Planck_intf.S
  with type Str.elem = S.elem
  and  module Str.Pos = S.Pos
  and  type Str.attr = S.attr
  and  type 'a t = S.t -> ('a * S.t, S.Pos.t * string) Result.t
(** Functor [Make] takes a stream implementation [S] and create a monadic parser module for [S].
    The parse uses the element, position and attribute from [S].
    The monad is a function type [S.t -> ('a * S.t, S.Pos.t * string) Result.t].
*)
