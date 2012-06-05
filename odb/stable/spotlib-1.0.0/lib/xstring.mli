val is_prefix : ?from:int -> prefix:string -> string -> bool
val is_postfix : postfix:string -> string -> bool
val index_string_from : string -> int -> string -> int (* may raise Not_found *)
