open Camlp4.PreCast
open Syntax

EXTEND Gram
  expr: BEFORE "simple"
    [[ s = STRING ->
         let lexbuf = Lexing.from_string s in
         Xstrp4_batteries.M.interpolated_expr lexbuf _loc
     ]];

END
;;

