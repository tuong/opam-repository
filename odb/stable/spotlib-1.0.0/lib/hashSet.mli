(* Poorman's implementation of polymorphic hash set *)
type 'a t
val create : int -> 'a t
val mem : 'a t -> 'a -> bool
val add : 'a t -> 'a -> unit
val remove : 'a t -> 'a -> unit

