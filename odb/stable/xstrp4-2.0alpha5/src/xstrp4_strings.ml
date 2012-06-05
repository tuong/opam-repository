open Camlp4.PreCast
open Syntax

(** Use the default implementation as-is *)
module M = Xstrp4.Make(Xstrp4.Default_implementation)(Xstrp4.Default_lexer)

EXTEND Gram
  expr: BEFORE "simple"
    [[ s = STRING ->
         let lexbuf = Lexing.from_string s in
         M.interpolated_expr lexbuf _loc
     ]];

END
;;

