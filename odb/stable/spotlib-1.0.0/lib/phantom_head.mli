(**
   Opening this module like Spotlib.Spot.Phantom is NOT recommended. 
   Instead, open Spotlib.Spot.Phantom.Open. Using a module alias is also helpful:
   
   module P = Spotlib.Spot.Phantom
   open P.Open
*)
type ('phantom, 'cont) t

(* Rather than open Phantom, I recommend to open Phantom.Open *)
module Open : sig
  type unknown 
  val unknown : unknown
  val (!<) : ('a, 'cont) t -> 'cont
  (** Forget the phantom *)
  val (!>) : 'cont -> (unknown, 'cont) t
  (** Safe lift up with the unknown phantom *)
  val (!?) : ('a, 'cont) t -> (unknown, 'cont) t
  (** Forget the phantom *)
end
type unknown = Open.unknown
val unknown : Open.unknown
val (!<) : ('a, 'cont) t -> 'cont
val (!>) : 'cont -> (unknown, 'cont) t
val (!?) : ('a, 'cont) t -> (unknown, 'cont) t

val unsafe : 'cont -> ('unsafe, 'cont) t
(** [unsafe v] lifts up [v] of [elt] to one with any phantom. Use with care. *)
val magic : ('a, 'cont) t -> ('unsafe, 'cont) t
(** [magic v] changes the phantom ov [v]. Use with care. *)

val map : ('cont -> 'cont2) -> ('a, 'cont) t -> ('a, 'cont2) t
val combine : 'tag -> ('a, 'cont) t -> ('a, 'tag * 'cont) t

type ('phantom, 'cont) ts
(** phantom heterogeneous 'cont list *)

module List : sig
  val unsafe_list : 'cont list -> ('unsafe, 'cont) ts
  (** [unsafe_list ls] lifts up [ls] of [elt list] to a list with any phantom. Use with care. *)
  val to_list : ('a, 'cont) ts -> 'cont list
  val to_unknown_list : ('a, 'cont) ts -> (unknown, 'cont) t list
  val to_array : ('a, 'cont) ts -> 'cont array
  val to_unknown_array : ('a, 'cont) ts -> (unknown, 'cont) t array
  val of_unknown_list : (unknown, 'cont) t list -> (unknown, 'cont) ts
  val of_unknown_array : (unknown, 'cont) t array -> (unknown, 'cont) ts
  val unsafe_of_list : 'cont list -> ('unsafe, 'cont) ts
  val unsafe_of_array : 'cont array -> ('unsafe, 'cont) ts
  
  val length : ('a, 'cont) ts -> int
  val map : ('cont -> 'cont2) -> ('a, 'cont) ts -> ('a, 'cont2) ts
  val combine : 'tag list -> ('a, 'cont) ts -> ('a, ('tag * 'cont)) ts

  type ('phantom, 'cont) t = ('phantom, 'cont) ts
end

(* This encoding is correct only if the parameter cannot be the unit or a tuple *)
