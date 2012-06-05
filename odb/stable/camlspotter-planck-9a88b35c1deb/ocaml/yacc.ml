open Sexplib.Conv

module Rule = struct
  type t = {
    nonterminal : string;
    cases : case list;
    mutable leftrec : leftrec
  }

  and case = {
    symbols : string list;
    prec : string option;
    ocaml : string ;
    mutable case_leftrec : leftrec
  } 

  and leftrec = [ `NonMutual | `Mutual of t list | `NonRecursive | `Unknown ]

  with sexp

  let compare x y = compare x.nonterminal y.nonterminal

  let hash x = Hashtbl.hash x.nonterminal

  let equal x y = x.nonterminal = y.nonterminal

  let is_direct_leftrec t =
    List.mem t.nonterminal 
      (List.concat (List.map (fun c -> 
        match c.symbols with
        | [] -> []
        | x::_ -> [x]) t.cases))
end

(* leftrec mutual recursion analysis *)  
module Rules = struct

  module G = struct
    type t = Rule.t list
    module V = Rule
    let iter_vertex = List.iter
    let iter_succ f rules rule =
      List.iter (fun c -> 
        match c.Rule.symbols with
        | [] -> ()
        | head::_ ->
            let r =
              try
                Some (List.find (fun r -> r.Rule.nonterminal = head) rules )
              with
              | Not_found -> None
            in
            match r with
            | Some r -> f r
            | None -> ()) rule.Rule.cases
  end

  include Graph.Components.Make(G)

  let analyze rules = 
    let scc_query = snd (scc rules) in
    let sccs = scc_array rules in
    List.iter (fun r -> 
      let sccs = sccs.(scc_query r) in
      r.Rule.leftrec <-
        begin match sccs with
        | [] -> assert false
        | [_] -> if Rule.is_direct_leftrec r then `NonMutual else `NonRecursive
        | _ -> `Mutual sccs;
        end;
      List.iter (fun c ->
        c.Rule.case_leftrec <-
          begin match r.Rule.leftrec with
          | `Unknown -> assert false
          | `NonRecursive -> `NonRecursive
          | `NonMutual ->
              begin match c.Rule.symbols with
              | [] -> `NonRecursive
              | s::_ -> if s = r.Rule.nonterminal then `NonMutual else `NonRecursive
              end
          | `Mutual _ ->
              begin match c.Rule.symbols with
              | [] -> `NonRecursive
              | s::_ -> if List.exists (fun r -> r.Rule.nonterminal = s) sccs then `Mutual sccs else `NonRecursive
              end
          end
      ) r.Rule.cases
            
    ) rules
end

module Decl = struct
  type t = 
    | Token of string option * string list
    | Start of string list
    | Type of string * string list
    | Left of string list
    | Right of string list
    | Nonassoc of string list
  with sexp
end

module Yacc : sig 
  type t = private { 
    header : string;
    decls : Decl.t list;
    rules : Rule.t list;
    trailer : string
  } with sexp

  val create : header:string -> decls:Decl.t list -> rules:Rules.G.t -> trailer:string -> t

end = struct
  type t = { 
    header : string;
    decls : Decl.t list;
    rules : Rule.t list;
    trailer : string
  } with sexp

  let create ~header ~decls ~rules ~trailer = 
    Rules.analyze rules;
    { header = header;
      decls = decls;
      rules = rules;
      trailer = trailer }
end

include Yacc
