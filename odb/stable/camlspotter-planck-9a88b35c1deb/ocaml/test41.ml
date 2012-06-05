type 'a t = int 
(* ocaml: " 'a t = int" *)
(* type_parameters consumed something, therefore the space is included 

    type_declaration: type_parameters LIDENT type_kind constraints
      rhs_loc 1 -- 4-7                => reported as 5-7
      symbol_rloc () -- 4-15          => reported as 5-15

   type_parameters: type_parameter
      rhs_loc 1 --   4-7              => reported as 5-7!
      symbol_rloc -- 4-7              => reported as 5-7!

   type_parameter:     type_variance QUOTE ident 
      rhs_loc 1 -- 4-4                => reported as 4-4
      symbol_rloc -- 5-7              => reported as 5-7
*)

(* conclusion

   - If a symbol consume no token, it produces empty location using the location of lastly consumed token
   - Symbol_rloc is created from rhs_loc i but ignore empty locations
*)
