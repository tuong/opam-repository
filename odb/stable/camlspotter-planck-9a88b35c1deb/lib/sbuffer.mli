(** Buffered char stream

    Buffered char stream is a specialized stream for chars which provides
    efficient operations of [substr].

    See a use example in test/expr.ml
*)

open Stream_intf

type buf
(** ADT for string buffer *)

val default_buf : buf
val position_of_buf : buf -> Position.File.t

(** Extend(Base) extends Base with buffered string operations. 
    Typical usage is:

    module MyStream = struct
      module Base = ... (* creation of Base *)
      include Base
      include Sbuffer.Extend(Base)
    end
*)

module Extend(Base : sig
  include S
  with type elem = char
  and  type Pos.t = Position.File.t
  (** Sbuffer is based on a stream whose element is [char].
      [attr] is implementation dependent.
  *)
  val create_attr : buf -> attr
  val buf : t -> buf
end) : sig

  open Base

  val create : buf -> t
  (** Create a stream from a buffer *)

  val substr : t -> int -> int -> string * t
  (** [substr t from len] returns a substring of [len] chars from [t]
      from its absolute char position [from], and a stream which starts at the end of the substring.

      It may raise an exception if [from] and [len] point outside of the stream *)

  val takeWhile : (char -> bool) -> t -> string * t
  (** [takeWhile p t] returns the longest prefix of [t] all whose characters suffices the predicate [p],
      and the stream which starts at the end of the prefix.
  *)

  val bytes : t -> int
  (** [bytes t] returns the head position of the stream as the nubmer of bytes. *)

  val from_string : filename:string -> string -> t
  val from_chan : filename:string -> in_channel -> t
  (** Creation functions of buffered streams *)

  val set_position : t -> Position.File.t -> t
  (** [set_position t pos] returns the same stream as [t] but with the head position is changed by [pos] *)
end
