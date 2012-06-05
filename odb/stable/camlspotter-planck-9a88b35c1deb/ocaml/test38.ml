let _ = function A _ | B -> 0   (* wrongly parsed as A <_ | B> *)
(* %prec prec_constr_appl *)

