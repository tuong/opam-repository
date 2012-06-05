open Sexplib.Conv

open Planck
open Input.Parser

(* No nested comment support *)
let c_comment = perform 
  string "/*"; 
  let rec loop () = string "*/" <!> (take_ >>= loop) in
  loop ()

let blank = one_of [' '; '\t'; '\n'; '\r']

(* simply ignore comments *)
let blank_or_c_comment = (?+ blank) <!> c_comment

(* preheader can only have C comments and blanks *)
let pre_header = ?* blank_or_c_comment

(* it does not check whether % comes at the beginning of a line. *)
let header = perform
  string "%{";
  \ prerr_endline "header begin";
  let rec loop () = 
    (perform ?* (ignore blank <!> Lex.comment ()); string "%}") 
    <!> (Lex.ocaml_token >>= fun _v -> loop ()) 
  in
  matched (loop ())

module Decl = struct

  open Yacc.Decl

  let constr = Lex.uident
  let symbol = Lex.lident <!> Lex.uident

  let percent_key s = perform
    token '%'; ?* blank_or_c_comment; string s

  let type_ = perform
    token '<'; 
    critical ((perform
                ?* blank_or_c_comment;
                s <-- matched (?+ (tokenp ((<>) '>')));
                token '>';
                return s) <?!> "error at type argument" )

  let decl_token = perform
    percent_key "token"; 
    critical ((
      (perform
          ?* blank_or_c_comment;
          typ <-- type_; ?* blank_or_c_comment;
          constrs <-- list_with_sep ~sep:(?+ blank_or_c_comment) constr;
          return (Token (Some typ, constrs)))
       <!> (perform
              ?+ blank_or_c_comment;
              constrs <-- list_with_sep ~sep:(?+ blank_or_c_comment) constr;
              return (Token (None, constrs)))
    ) <?!> "%token")

  let type_ = perform
    percent_key "type"; 
    critical (( perform
      ?* blank_or_c_comment;
      typ <-- type_; ?* blank_or_c_comment;
      symbols <-- list_with_sep ~sep:(?+ blank_or_c_comment) symbol;
      return (Type (typ, symbols))
    ) <?!> "%type") 

  let followed_by_symbols name c = perform
    percent_key name; 
    critical ( ( perform
      ?+ blank_or_c_comment;
      symbols <-- list_with_sep ~sep:(?+ blank_or_c_comment) symbol;
      return (c symbols)
    ) <?!> ("%" ^ name) )

  let start    = followed_by_symbols "start" (fun x -> Start x)
  let left     = followed_by_symbols "left" (fun x -> Left x)
  let right    = followed_by_symbols "right" (fun x -> Left x)
  let nonassoc = followed_by_symbols "nonassoc" (fun x -> Nonassoc x)

  let rec parse () = 
    decl_token <!> start <!> type_ <!> left <!> right <!> nonassoc
      <!> (perform ?+ blank_or_c_comment; parse ())
end

module Rule = struct

  open Yacc.Rule

  let prec = perform
    token '%'; ?* blank_or_c_comment;
    string "prec"; 
    critical ((perform
      ?+ blank_or_c_comment;
      Lex.lident
    ) <?!> "%prec")

(* CR jfuruse: comparison is not simple I am afraid *)
(*
  let rec compare_rev_symbols syms1 syms2 = 
    match syms1, syms2 with
    | [], [] -> 0
    | [], _ -> 1
    | _, [] -> -1
    | s1::syms1, s2::syms2 ->
        match compare s1 s2 with
        | (1 | -1 as res) -> res
        | 0 -> compare_rev_symbols syms1 syms2
        | _ -> assert false

  let compare_case case1 case2 = 
    compare_rev_symbols (List.rev case1.symbols) (List.rev case2.symbols)
*)

  let rec compare_symbols syms1 syms2 = 
    match syms1, syms2 with
    | [], [] -> 0
    | [], _ -> 1
    | _, [] -> -1
    | s1::syms1, s2::syms2 ->
        match Pervasives.compare s1 s2 with
        | (1 | -1 as res) -> res (* Tokens (capitals) should come first *)
        | 0 -> compare_symbols syms1 syms2
        | _ -> assert false

  let compare_case case1 case2 = 
    match Pervasives.compare (List.length case1.symbols) (List.length case2.symbols) with
    | (1 | -1 as res) -> - res
    | 0 -> compare_symbols case1.symbols case2.symbols
    | _ -> assert false

  let symbol = Lex.lident <!> Lex.uident

  let ocaml = perform
    token '{';
    critical ((perform
      let rec loop lev = 
        (perform ?+ blank; loop lev)
        <!> (perform
               token '{';
               loop (lev+1))
        <!> (perform
               if lev > 0 then begin perform 
                 token '}'; 
                 loop (lev-1) 
               end else token '}')
        <!> (perform
               ignore Lex.ocaml_token;
               loop lev)
      in
      matched (loop 0)
    ) <?!> "ocaml code")
      
  let case = perform
    symbols <-- (list_with_sep ~sep:(?+ blank_or_c_comment) symbol <!> return []);
    ?* blank_or_c_comment;
    prec_opt <-- option prec; ?* blank_or_c_comment;
    ocaml <-- ocaml; 
    ?* blank_or_c_comment;
    return { symbols = symbols; prec = prec_opt; ocaml = ocaml; case_leftrec = `Unknown }
      
  let parse = perform
    nonterminal <-- Lex.lident; 
    critical ((perform
      ?* blank_or_c_comment;
      token ':'; ?* blank_or_c_comment;
      cases <-- list_with_sep ~optional_head:true ~sep:(perform ?* blank_or_c_comment; token '|'; ?* blank_or_c_comment) case;
      token ';'; ?* blank_or_c_comment;
      return { nonterminal = nonterminal; cases = cases; leftrec = `Unknown }
    ) <?!> ("error at rule " ^ nonterminal) )
end

      


(* it does not check whether % comes at the beginning of a line. *)
let trailer = matched (?* (Lex.ocaml_token >>= function
                             | Token.EOF, _ -> error "eos reached"
                             | v, _ -> return v))

type decls = Yacc.Decl.t list with sexp

let parse = perform
  \ prerr_endline "pre_header";
  pre_header;
  \ prerr_endline "done pre_header";
  h <-- header;
  \ prerr_endline "done header";
  decls <-- ?** (Decl.parse ()); ?* blank_or_c_comment;
  \ prerr_endline "done decls";
  string "%%"; ?* blank_or_c_comment;
  \ prerr_endline "rules...";
  rules <-- ?** Rule.parse; ?* blank_or_c_comment;
  \ prerr_endline "rules done";
  string "%%"; ?* blank_or_c_comment;
  \ prerr_endline "trailer...";
  tr <-- trailer;
  \ prerr_endline "trailer done";
  eos;
  return (Yacc.create ~header:h ~decls ~rules ~trailer:tr)

let parse st = try parse st with Critical_error (pos, mes) ->
  Result.Error (pos, mes)
