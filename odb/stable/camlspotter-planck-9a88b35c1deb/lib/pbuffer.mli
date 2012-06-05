(** Monadic parser for buffered char streams *)

open Sbuffer

(** Extend(Str)(Base) extends Base for Str with buffered string operations
    Typical usage is:

    module MyParser = struct
      module Base = ...(Str)... (* creation of Base *)
      include Base
      include Pbuffer.Extend(Str)(Base)
    end

    It includes the extensions of Pchar.Extend. If you use Pbuffer.Extend, 
    no need of using Pchar.Extend.

    See a use example in test/expr.ml
*)

module Extend(Sbuffer : sig
  include Stream_intf.S
  with type elem = char
  and  type Pos.t = Position.File.t
  val substr : t -> int -> int -> string * t
  val takeWhile : (char -> bool) -> t -> string * t
  val bytes : t -> int
end)(Base : Planck_intf.S 
     with type Str.elem = Sbuffer.elem
     and  type Str.attr = Sbuffer.attr
     and  type Str.Pos.t = Sbuffer.Pos.t) : sig

  (* CR jfuruse: we cannot write Base : Planck_intf.S with module Str = Sbuffer ! *)

  open Base

  val prefix : int -> string t
  (** fast retrieval of the prefix string of the given length *)

  val takeWhile : (char -> bool) -> string t
  (** [takeWhile p] matches the longest prefix string where all the chars hold [p] *)

  val ( ??** ) : (char -> bool) -> string t
  val ( ??* ) : (char -> bool) -> unit t
  (** Same as [Planck_intf.( ?** )] and [Planck_intf.( ?* )] but specialized for char stream *)   

  val string : string -> unit t
  (** Efficient version of [Pchar.string] using Sbuffer. 
      [string s] succeeds if the prefix of the stream is [s]. *)

  val chars_to_string : char list t -> string t
  (** Type conversion from [char list t] to [string t].
      For efficiency reason, users is not encouraged to use this function. Use [matched] instead.
  *)

  val matched : unit t -> string t
  (** [matched t] returns the matched string part of [t].
      It is far more efficient than manual concatenation of chars using [chars_to_string].
  *)

  val with_matched : 'a t -> ('a * string) t
  (** [with_matched t] returns the matched string part of [t] 
      along with the original result of [t]. *)

  val ( </> ) : 'a t -> 'a t -> 'a t
  (** Longest match. [a </> b] succeeds if either [a] or [b] succeeds.
      If the both of [a] and [b] succeeds, the longer match is used as the result of [a </> b].

      Note: [a </> b] runs both [a] and [b]. Too many uses of [</>] affects the parsing performance badly.
  *)
end
