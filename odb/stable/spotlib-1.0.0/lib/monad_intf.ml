(** Minimum monad interface *)
module type S = sig
  type +'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type Open = sig
  type +'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val return : 'a -> 'a t
    
    (** Applicative style binops *)
    
  val (^<$>) : ('a -> 'b) -> 'a t -> 'b t
    (** same as map, <$> in Haskell *)
    
  val (/<*>) : ('a -> 'b) t -> 'a t -> 'b t
  (** <*> in Haskell *)
end

module type T = sig
  include Open
  module Open : Open with type 'a t = 'a t

  val map : f:('a -> 'b) -> 'a t -> 'b t
  (** fmap in Haskell *)

  val ignore : 'a t -> unit t (* CR jfuruse: ignore is confusing with Pervasives.ignore. Probably it should be superceded by void *)
  val void : 'a t -> unit t

  val seq : 'a t list -> 'a list t
    (** sequence in Haskell. Not tail recursive. *)
  val seq_ : unit t list -> unit t
    (** sequence_ in Haskell. Tail recursive. *)
  val mapM : ('a -> 'b t) -> 'a list -> 'b list t
    (** Not tail recursive *)
  val iteri : (int -> 'a -> unit t) -> 'a list -> unit t
  val for_ : int -> int -> (int -> unit t) -> unit t
end

module type S2 = sig
  type ('a, 'z) t
  val return : 'a -> ('a, 'z) t
  val bind : ('a, 'z) t -> ('a -> ('b, 'z) t) -> ('b, 'z) t
end

module type Open2 = sig
  type ('a, 'z) t
  val bind : ('a, 'z) t -> ('a -> ('b, 'z) t) -> ('b, 'z) t
  val ( >>= ) : ('a, 'z) t -> ('a -> ('b, 'z) t) -> ('b, 'z) t
  val ( >>| ) : ('a, 'z) t -> ('a -> 'b) -> ('b, 'z) t
  val return : 'a -> ('a, 'z) t
    
    (** Applicative style binops *)
    
  val (^<$>) : ('a -> 'b) -> ('a, 'z) t -> ('b, 'z) t
    (** same as map, <$> in Haskell *)
    
  val (/<*>) : ('a -> 'b, 'z) t -> ('a, 'z) t -> ('b, 'z) t
  (** <*> in Haskell *)
end

module type T2 = sig
  include Open2
  module Open : Open2 with type ('a, 'z) t = ('a, 'z) t

  val map : f:('a -> 'b) -> ('a, 'z) t -> ('b, 'z) t
    (** fmap in Haskell *)

  val ignore : ('a, 'z) t -> (unit, 'z) t  (* CR jfuruse: ignore is confusing with Pervasives.ignore. Probably it should be superceded by void *)
  val void : ('a, 'z) t -> (unit, 'z) t

  val seq : ('a, 'z) t list -> ('a list, 'z) t
    (** sequence in Haskell. Not tail recursive. *)
  val seq_unit : (unit, 'z) t list -> (unit, 'z) t
    (** sequence_ in Haskell. Tail recursive. *)
  val mapM : ('a -> ('b, 'z) t) -> 'a list -> ('b list, 'z) t
    (** Not tail recursive *)
  val iteri : (int -> 'a -> (unit, 'z) t) -> 'a list -> (unit, 'z) t
  val for_ : int -> int -> (int -> (unit, 'z) t) -> (unit, 'z) t
end
