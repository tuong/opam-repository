open Camlp4.PreCast
open Syntax

(** Use the default implementation as-is *)
module Here = Xstrp4.Make(Xstrp4.Default_implementation)(Xstrp4.Default_lexer)

let here_expr _loc _loc_name s =
  let lexbuf = Lexing.from_string s in
  Here.interpolated_expr lexbuf _loc
;;

(** Create a <:here< ... >> quotation for the default interpolation scheme. *)
Quotation.add
  "here"
  Quotation.DynAst.expr_tag
  here_expr
;;

