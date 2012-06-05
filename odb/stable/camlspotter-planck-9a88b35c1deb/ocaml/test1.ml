type t = Location.t
(* ocaml: "t = Location.t" *)
(* rule type_parameters consumes nothing, so the space is not included 

    type_parameters LIDENT type_kind constraints
      rhs_loc 1 -- 4-4             => reported as 4-4
      symbol_rloc () -- 5-19       => reported as 5-19 

   type_parameters: /*empty*/    
      rhs_loc 1 -- not defined
      symbol_rloc -- 4-4           => reported as 4-4

*)
