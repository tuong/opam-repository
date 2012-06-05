val iteri : (int -> 'a -> 'b) -> 'a list -> unit
val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

val from_to : int -> int -> int list
  (** [from_to f t = [f..t]] *)
