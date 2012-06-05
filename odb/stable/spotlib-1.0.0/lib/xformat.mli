open Format

type t = formatter

val stdout : t
val stderr : t

val list : (t -> unit) -> (t -> 'a -> unit) -> t -> 'a list -> unit
