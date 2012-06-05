open Camlp4.PreCast
open Syntax

let batt_expr _loc _loc_name s =
  let lexbuf = Lexing.from_string s in
  Xstrp4_batteries.M.interpolated_expr lexbuf _loc
;;

(** Create a <:batt< ... >> quotation for our interpolations *)
Quotation.add
  "batt"
  Quotation.DynAst.expr_tag
  batt_expr
;;
