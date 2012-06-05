(** Monadic parser for char stream. 

    For better performance, Pbuffer is encouraged to use, instead of this Pchar module.
 *)

(** Extend(Str)(Base) extends Base for Str with string operations
    Typical usage is:

    module MyParser = struct
      module Base = ...(Str)... (* creation of Base *)
      include Base
      include Pchar.Extend(Str)(Base)
    end
*)

module Extend(Str : Stream_intf.S with type elem = char)
             (Base : Planck_intf.S 
              with type Str.elem = char
              and  type Str.attr = Str.attr
              and  type Str.Pos.t = Str.Pos.t) :
sig
  val string : string -> unit Base.t
  (** [string s] succeeds if the prefix of the stream is [s]. *)

  val chars_to_string : char list Base.t -> string Base.t
  (** Type conversion from [char list t] to [string t]. *)
end
