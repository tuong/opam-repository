open Spotlib.Spot
open Planck

open Ocamlyacc

let rec parse stream = 
  match Ocamlyacc.parse stream with
  | Result.Ok (v, _stream') ->
(*
      let show t = Sexplib.Sexp.to_string_hum (Ocamlyacc.sexp_of_t t) in
      prerr_endline (show v);
*)
      v
  | Result.Error (pos, s) -> 
      Format.eprintf "%a: syntax error: %s@." Position.File.format pos s;
      raise Exit

(* $ => "v_" *)
let replace_dollar_n s =
  let buf = Buffer.create (String.length s * 2) in
  let nums = ref [] in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '$' -> 
        let j = 
          let rec scan j = 
            if j = String.length s then j
            else match s.[j] with
            | '0'..'9' -> scan (j+1)
            | _ -> j
          in
          scan (i+1)
        in
        nums := int_of_string (String.sub s (i+1) (j-i-1)) :: !nums;
        Buffer.add_string buf "v_"
    | c -> Buffer.add_char buf c
  done;
  Buffer.contents buf, !nums

open Format

let tokens = Hashtbl.create 107

let symcode ppf case_is_leftrec used_ids i sym =
  let subst ppf =
    if List.mem i used_ids then fprintf ppf "v_%d <-- " i
    else fprintf ppf "_v_%d <-- " i
  in
  try 
    match sym with
    | "error" -> fprintf ppf "take_ (* error *)"
    | "EOF" -> fprintf ppf "eos"
    | _ -> 
        match Hashtbl.find tokens sym with
        | None -> fprintf ppf "token %s" sym
        | Some _ -> fprintf ppf "%tget_%s" subst sym
  with
  | Not_found -> 
      fprintf ppf "%tself#%s %s" subst sym (if i = 1 && case_is_leftrec then "(* leftrec *)" else "")

let process v = 
  
  Format.eprintf "#rules=%d@." (List.length v.Yacc.rules);
  let sccs = Yacc.Rules.scc_list v.Yacc.rules in
  List.iter (fun scc ->
    match scc with
    | [] -> ()
    | [rule] when not (Yacc.Rule.is_direct_leftrec rule) -> ()
    | _ ->
        Format.eprintf "SCC %d: @[%a@]@."
          (List.length scc)
          (Format.list (fun ppf -> Format.fprintf ppf "@ ")
             (fun ppf rule -> Format.fprintf ppf "%s" rule.Yacc.Rule.nonterminal)) scc
    ) sccs;

  printf "(* header *)
open Parsing
open Token
open Token.Parser
open Planck
open Plphelper
@.";

  printf "%s@." v.Yacc.header;
  printf "(* /header *)@.@.";

  printf "(* declarations *)@.";
  (list (fun _ppf -> ())
       (fun ppf d -> 
         match d with
         | Yacc.Decl.Token (Some _, tkns) ->
             List.iter (fun tkn ->
               fprintf ppf "let get_%s = token_result (function (%s v) -> Result.Ok v | _ -> Result.Error \"expected %s\")@." tkn tkn tkn) tkns
         | Yacc.Decl.Token (None, _tkns) -> ()
         | _ -> fprintf ppf "(* %a *)@." (Sexplib.Sexp.pp_hum) (Yacc.Decl.sexp_of_t d);
       )) stdout v.Yacc.decls;
  printf "(* /declarations *)@.@.";

  List.iter (function 
    | Yacc.Decl.Token (typopt, tkns) -> 
        List.iter (fun tkn -> Hashtbl.add tokens tkn typopt) tkns
    | _ -> ()) v.Yacc.decls;

  printf "(* rules *)@.@.";

  printf "class rules = object (self)@.";

  List.iter (fun r ->

    match r.Yacc.Rule.leftrec with
    | `Unknown -> assert false
    | `NonRecursive ->

        printf "  method %s = rule %S (fun () -> dummy@." r.Yacc.Rule.nonterminal r.Yacc.Rule.nonterminal;
        List.iteri (fun i c ->
          let ocaml_code, used_ids = replace_dollar_n c.Yacc.Rule.ocaml in
          printf "    <!> case 1 \"%s_%d\" (fun () -> perform@." r.Yacc.Rule.nonterminal i;
          printf "@.";
          if c.Yacc.Rule.symbols = [] then printf "           (* empty *)@."
          else
            printf "           @[<v>%a@];@." 
              (list (fun ppf -> fprintf ppf ";@,") (fun ppf (i,sym) -> symcode ppf false used_ids i sym))
              (List.mapi (fun i sym -> (i+1,sym)) c.Yacc.Rule.symbols);
          (match c.Yacc.Rule.prec with
          | None -> ()
          | Some sym -> printf "           (* %%prec %s *)@." sym);
          printf "@.";
          printf "           return (fun () -> %s))@.@." ocaml_code)
          (List.sort Ocamlyacc.Rule.compare_case r.Yacc.Rule.cases);
        printf "      )@.@.";
             
    | `Mutual _ | `NonMutual ->

        printf "  method %s = leftrec %S self#%s_nonleftrec self#%s_leftrec@.@."
          r.Yacc.Rule.nonterminal r.Yacc.Rule.nonterminal
          r.Yacc.Rule.nonterminal r.Yacc.Rule.nonterminal;

        printf "  method %s_nonleftrec = (dummy@." r.Yacc.Rule.nonterminal;
        List.iteri (fun i c ->
          let ocaml_code, used_ids = replace_dollar_n c.Yacc.Rule.ocaml in
          printf "    <!> case 1 \"%s_nonleftrec_%d\" (fun () -> perform@." r.Yacc.Rule.nonterminal i;
          printf "@.";
          if c.Yacc.Rule.symbols = [] then printf "           (* empty *)@."
          else
            printf "           @[<v>%a@];@." 
              (list (fun ppf -> fprintf ppf ";@,") (fun ppf (i,sym) -> symcode ppf false used_ids i sym))
              (List.mapi (fun i sym -> (i+1,sym)) c.Yacc.Rule.symbols);
          (match c.Yacc.Rule.prec with
          | None -> ()
          | Some sym -> printf "           (* %%prec %s *)@." sym);
          printf "@.";
          printf "           return (fun () -> %s))@.@." ocaml_code)
          (List.sort Ocamlyacc.Rule.compare_case (List.filter (fun c -> c.Yacc.Rule.case_leftrec = `NonRecursive) r.Yacc.Rule.cases));
        printf "      )@.@.";
             
        printf "  method %s_leftrec v_1 = (dummy@." r.Yacc.Rule.nonterminal;
        List.iteri (fun i c ->
          let ocaml_code, used_ids = replace_dollar_n c.Yacc.Rule.ocaml in
          let is_case_mutual =
            match c.Yacc.Rule.symbols with
            | s::_ -> s <> r.Yacc.Rule.nonterminal
            | _ -> false
          in
          if is_case_mutual then printf "(* MUTUAL LEFT REC @.";
          printf "    <!> case 2 \"%s_leftrec_%d\" (fun () -> perform@." r.Yacc.Rule.nonterminal i;
          printf "@.";
          if is_case_mutual then begin
            let symbols = c.Yacc.Rule.symbols in
            if symbols = [] then printf "           (* empty *)@."
            else
              printf "           @[<v>%a@];@." 
                (list (fun ppf -> fprintf ppf ";@,") (fun ppf (i,sym) -> symcode ppf false used_ids i sym))
                (List.mapi (fun i sym -> (i+1,sym)) symbols);
            (match c.Yacc.Rule.prec with
            | None -> ()
            | Some sym -> printf "           (* %%prec %s *)@." sym);
            printf "@.";
            printf "           return (fun () -> %s))@.@."
              ocaml_code
          end else begin
            let symbols = List.tl c.Yacc.Rule.symbols in
            if symbols = [] then printf "           (* empty *)@."
            else
              printf "           @[<v>%a@];@." 
                (list (fun ppf -> fprintf ppf ";@,") (fun ppf (i,sym) -> symcode ppf false used_ids i sym))
                (List.mapi (fun i sym -> (i+2,sym)) symbols);
            (match c.Yacc.Rule.prec with
            | None -> ()
            | Some sym -> printf "           (* %%prec %s *)@." sym);
            printf "@.";
            printf "           return (fun () -> %s))@.@." 
              ocaml_code
          end;
          if is_case_mutual then printf "*)@.")
          (List.sort Ocamlyacc.Rule.compare_case (List.filter (fun c -> c.Yacc.Rule.case_leftrec <> `NonRecursive) r.Yacc.Rule.cases));
        printf "      )@.@.";

  ) v.Yacc.rules;

  printf "end@.";
    
  printf "(* /rules *)@.@.";

  printf "(* trailer *)@.%s@.(* /trailer *)@." v.Yacc.trailer

let _ = Arg.parse [] (fun x ->
  let ic = open_in x in
  let stream = Input.Stream.from_chan ~filename:"" ic in
  let v = parse stream in
  close_in ic;
  process v;
  ) "ocamlyacctest files"

