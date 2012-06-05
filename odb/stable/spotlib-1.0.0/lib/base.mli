val failwithf : ('a, unit, string, 'b) format4 -> 'a

val memoize : ('c -> 'd) -> 'c -> 'd

val (^.) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
  (** funciton composition: Haskell's (.) *)
val (^$) : ('a -> 'b) -> 'a -> 'b
  (** Haskell's ($) *)

val protect : ('a -> 'b) -> 'a -> finally: (unit -> unit) -> 'b

val with_time : ('a -> 'b) -> 'a -> 'b * float
  (** simple profiling *)
 
