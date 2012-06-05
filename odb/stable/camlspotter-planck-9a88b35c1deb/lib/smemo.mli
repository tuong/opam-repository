(** Stream module extention for memoization 

    This module is very preliminary.
*)

(** Extend(Base) extends Base with memoization over each stream element.
    Typical usage is:

    module MyStream = struct
      module Base = ... (* creation of Base *)
      include Base
      include Smemo.Extend(Base)
    end

    See a use example in ocaml/token.ml
*)

type memo_result = (Obj.t, exn) Result.t
(** Type unsafe representation of generic memoization table *)

module Extend(Base : sig
  include Stream_intf.S
  module Memo : Hashtbl.S (** Memo is hashtable *)    
  val memo : t -> memo_result Memo.t
  (** Memo table must be attached to each stream position, and it must be retrievable. *)    
end) : sig
  open Base
  val memoize : Memo.key -> (t -> 'a) -> t -> 'a
  (** [memoize key f] creates memoized version of function [f] with [key]. 
      Function identification is not by the pointer equality of functions, but keys.
  *)
end
