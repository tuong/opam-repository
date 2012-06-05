open Planck
open Input.Parser

val ocaml_token : (Token.t * Position.Region.t) t
val ocaml_token_stream : Input.Stream.t -> Token.Stream.t
val skip_sharp_bang : unit t

(* higher interface *)
val int : Token.t t
val float : Token.t t
val int32 : Token.t t
val int64 : Token.t t
val nativeint : Token.t t

val string_ : Token.t t
  (* CR jfuruse: name is bad *)

val char : Token.t t

(** lower interfaces *)

val zero : unit t
val underscore : unit t
val decimal_char : char t 
  (** [0-9] *)
val bin_char : char t 
  (** [01] *)
val oct_char : char t
  (** [0-7] *)
val hex_char : char t
  (** [0-9a-fA-F] *)
val char_or_underscores : char t -> string t
  (** X(X|_)* *)

val decimal_literal : string t
val bin_literal : string t
val oct_literal : string t
val hex_literal : string t
val int_literal : string t
val float_literal : string t

val newline : unit t
val blank : unit t
val lowercase : char t
val uppercase : char t
val identchar : char t
val symbolchar : char t
val lident : string t
val uident : string t

val comment : unit -> unit t

