open Camlp4
open PreCast
open Ast

(** (start, end) range *)
type range_t = Lexing.position * Lexing.position

(** {1 Potential Clause Values} *)
type clause_t =
  | Literal of (string * range_t) (** Literal string *)
  | Variable of (string * range_t * string * range_t) (** ${expr, %printf-like-format} *)
  | Custom_variable of (string * range_t * string * range_t) (** [${expr, expr}] *)
  | Textend (** The end of the quoted text *)

