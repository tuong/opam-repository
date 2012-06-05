(* header *)
open Parsing
open Token
open Planck
open Token.Parser
open Plphelper


open Location
open Asttypes
open Longident
open Parsetree

let mktyp d =
  { ptyp_desc = d; ptyp_loc = symbol_rloc() }
let mkpat d =
  { ppat_desc = d; ppat_loc = symbol_rloc() }
let mkexp d =
  { pexp_desc = d; pexp_loc = symbol_rloc() }
let mkmty d =
  { pmty_desc = d; pmty_loc = symbol_rloc() }
let mksig d =
  { psig_desc = d; psig_loc = symbol_rloc() }
let mkmod d =
  { pmod_desc = d; pmod_loc = symbol_rloc() }
let mkstr d =
  { pstr_desc = d; pstr_loc = symbol_rloc() }
let mkfield d =
  { pfield_desc = d; pfield_loc = symbol_rloc() }
let mkclass d =
  { pcl_desc = d; pcl_loc = symbol_rloc() }
let mkcty d =
  { pcty_desc = d; pcty_loc = symbol_rloc() }

let reloc_pat x = { x with ppat_loc = symbol_rloc () };;
let reloc_exp x = { x with pexp_loc = symbol_rloc () };;

let mkoperator name pos =
  { pexp_desc = Pexp_ident(Lident name); pexp_loc = rhs_loc pos }

(*
  Ghost expressions and patterns:
  expressions and patterns that do not appear explicitely in the
  source file they have the loc_ghost flag set to true.
  Then the profiler will not try to instrument them and the
  -stypes option will not try to display their type.

  Every grammar rule that generates an element with a location must
  make at most one non-ghost element, the topmost one.

  How to tell whether your location must be ghost:
  A location corresponds to a range of characters in the source file.
  If the location contains a piece of code that is syntactically
  valid (according to the documentation), and corresponds to the
  AST node, then the location must be real; in all other cases,
  it must be ghost.
*)
let ghexp d = { pexp_desc = d; pexp_loc = symbol_gloc () };;
let ghpat d = { ppat_desc = d; ppat_loc = symbol_gloc () };;
let ghtyp d = { ptyp_desc = d; ptyp_loc = symbol_gloc () };;

let mkassert e =
  match e with
  | {pexp_desc = Pexp_construct (Lident "false", None, false); _ } ->
         mkexp (Pexp_assertfalse)
  | _ -> mkexp (Pexp_assert (e))
;;

let mkinfix arg1 name arg2 =
  mkexp(Pexp_apply(mkoperator name 2, ["", arg1; "", arg2]))

let neg_float_string f =
  if String.length f > 0 && String.unsafe_get f 0 = '-'
  then String.sub f 1 (String.length f - 1)
  else "-" ^ f

let mkuminus name arg =
  match name, arg.pexp_desc with
  | "-", Pexp_constant(Const_int n) ->
      mkexp(Pexp_constant(Const_int(-n)))
  | "-", Pexp_constant(Const_int32 n) ->
      mkexp(Pexp_constant(Const_int32(Int32.neg n)))
  | "-", Pexp_constant(Const_int64 n) ->
      mkexp(Pexp_constant(Const_int64(Int64.neg n)))
  | "-", Pexp_constant(Const_nativeint n) ->
      mkexp(Pexp_constant(Const_nativeint(Nativeint.neg n)))
  | ("-" | "-."), Pexp_constant(Const_float f) ->
      mkexp(Pexp_constant(Const_float(neg_float_string f)))
  | _ ->
      mkexp(Pexp_apply(mkoperator ("~" ^ name) 1, ["", arg]))

let mkuplus name arg =
  let desc = arg.pexp_desc in
  match name, desc with
  | "+", Pexp_constant(Const_int _)
  | "+", Pexp_constant(Const_int32 _)
  | "+", Pexp_constant(Const_int64 _)
  | "+", Pexp_constant(Const_nativeint _)
  | ("+" | "+."), Pexp_constant(Const_float _) -> mkexp desc
  | _ ->
      mkexp(Pexp_apply(mkoperator ("~" ^ name) 1, ["", arg]))

let rec mktailexp = function
    [] ->
      ghexp(Pexp_construct(Lident "[]", None, false))
  | e1 :: el ->
      let exp_el = mktailexp el in
      let l = {loc_start = e1.pexp_loc.loc_start;
               loc_end = exp_el.pexp_loc.loc_end;
               loc_ghost = true}
      in
      let arg = {pexp_desc = Pexp_tuple [e1; exp_el]; pexp_loc = l} in
      {pexp_desc = Pexp_construct(Lident "::", Some arg, false); pexp_loc = l}

let rec mktailpat = function
    [] ->
      ghpat(Ppat_construct(Lident "[]", None, false))
  | p1 :: pl ->
      let pat_pl = mktailpat pl in
      let l = {loc_start = p1.ppat_loc.loc_start;
               loc_end = pat_pl.ppat_loc.loc_end;
               loc_ghost = true}
      in
      let arg = {ppat_desc = Ppat_tuple [p1; pat_pl]; ppat_loc = l} in
      {ppat_desc = Ppat_construct(Lident "::", Some arg, false); ppat_loc = l}

let ghstrexp e =
  { pstr_desc = Pstr_eval e; pstr_loc = {e.pexp_loc with loc_ghost = true} }

let array_function str name =
  Ldot(Lident str, (if !Clflags.fast then "unsafe_" ^ name else name))

let rec deep_mkrangepat c1 c2 =
  if c1 = c2 then ghpat(Ppat_constant(Const_char c1)) else
  ghpat(Ppat_or(ghpat(Ppat_constant(Const_char c1)),
                deep_mkrangepat (Char.chr(Char.code c1 + 1)) c2))

let rec mkrangepat c1 c2 =
  if c1 > c2 then mkrangepat c2 c1 else
  if c1 = c2 then mkpat(Ppat_constant(Const_char c1)) else
  reloc_pat (deep_mkrangepat c1 c2)

let syntax_error () =
  raise Syntaxerr.Escape_error

let unclosed opening_name opening_num closing_name closing_num =
  raise(Syntaxerr.Error(Syntaxerr.Unclosed(rhs_loc opening_num, opening_name,
                                           rhs_loc closing_num, closing_name)))

let bigarray_function str name =
  Ldot(Ldot(Lident "Bigarray", str), name)

let bigarray_untuplify = function
    { pexp_desc = Pexp_tuple explist; _ } -> explist
  | exp -> [exp]

let bigarray_get arr arg =
  let get = if !Clflags.fast then "unsafe_get" else "get" in
  match bigarray_untuplify arg with
    [c1] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array1" get)),
                       ["", arr; "", c1]))
  | [c1;c2] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array2" get)),
                       ["", arr; "", c1; "", c2]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array3" get)),
                       ["", arr; "", c1; "", c2; "", c3]))
  | coords ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Genarray" "get")),
                       ["", arr; "", ghexp(Pexp_array coords)]))

let bigarray_set arr arg newval =
  let set = if !Clflags.fast then "unsafe_set" else "set" in
  match bigarray_untuplify arg with
    [c1] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array1" set)),
                       ["", arr; "", c1; "", newval]))
  | [c1;c2] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array2" set)),
                       ["", arr; "", c1; "", c2; "", newval]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array3" set)),
                       ["", arr; "", c1; "", c2; "", c3; "", newval]))
  | coords ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Genarray" "set")),
                       ["", arr;
                        "", ghexp(Pexp_array coords);
                        "", newval]))

let lapply p1 p2 =
  if !Clflags.applicative_functors
  then Lapply(p1, p2)
  else raise (Syntaxerr.Error(Syntaxerr.Applicative_path (symbol_rloc())))

let exp_of_label lbl =
  mkexp (Pexp_ident(Lident(Longident.last lbl)))

let pat_of_label lbl =
  mkpat (Ppat_var(Longident.last lbl))

(* ops! *)

let infix arg1 name arg2 f = 
  let reg2 = rhs_reg 2 in
  Op_prec.infix name (fun (arg1, reg1) (arg2, reg3) ->
    let pos_back = !symbol_rloc_ref in
    let cntr = push_rhs_tbl ("infix_" ^ name) in
    Spotlib.Spot.protect (fun () -> 
      add_rhs_pos reg1;
      add_rhs_pos reg2;
      add_rhs_pos reg3;
      prepare_symbol_rloc ();
      let pos = !symbol_rloc_ref in
      let res = f arg1 name arg2 in
      res, pos) () ~finally: (fun () -> 
        pop_rhs_tbl cntr;
        symbol_rloc_ref := pos_back
    );
  ) arg1 arg2

let mkinfix arg1 name arg2 = infix arg1 name arg2 mkinfix

(* CR jfuruse: can be merged with mku_op *)
let mkpat_prefix f name v_2 =
  let reg1 = rhs_reg 1 in
  Op_prec.prefix name (fun (arg, reg) ->
    let pos_back = !symbol_rloc_ref in
    let cntr = push_rhs_tbl ("mkpat_prefix_" ^ name) in
    Spotlib.Spot.protect (fun () ->
      add_rhs_pos reg1;
      add_rhs_pos reg;
      prepare_symbol_rloc ();
      let pos = !symbol_rloc_ref in
      let res = f arg in
      res, pos) () ~finally: (fun () -> 
        pop_rhs_tbl cntr;
        symbol_rloc_ref := pos_back);
  ) v_2

let mkpat_postfix f name v_1 =
  let reg2 = rhs_reg 2 in
  let reg3 = rhs_reg 3 in
  Op_prec.postfix name (fun (v_1, reg1) ->
    let pos_back = !symbol_rloc_ref in
    let cntr = push_rhs_tbl ("mkpat_postfix_" ^ name) in
    Spotlib.Spot.protect (fun () ->
      add_rhs_pos reg1;
      add_rhs_pos reg2;
      add_rhs_pos reg3;
      prepare_symbol_rloc ();
      let pos = !symbol_rloc_ref in
      let res = f v_1 in
      res, pos) () ~finally: (fun () -> 
        pop_rhs_tbl cntr;
        symbol_rloc_ref := pos_back);
  ) v_1

let mkpat_infix arg1 name arg2 f = infix arg1 name arg2 (fun arg1 _ arg2 -> f arg1 arg2) 

let build t = fst (Op_prec.build t)
let terminal e = Op_prec.terminal (e, !symbol_rloc_ref)
let mkexpX d = Op_prec.terminal (mkexp d, !symbol_rloc_ref)
let mkpatX d = Op_prec.terminal (mkpat d, !symbol_rloc_ref)
let mkassertX e = Op_prec.terminal (mkassert e, !symbol_rloc_ref)

let mkexp_prefix name arg f = 
  let reg1 = rhs_reg 1 in (* for the OP *)
  Op_prec.prefix name (fun (v_2, reg2) -> 
    let pos_back = !symbol_rloc_ref in
    let cntr = push_rhs_tbl ("mkexp_prefix_" ^ name) in
    Spotlib.Spot.protect (fun () ->
      add_rhs_pos reg1;
      add_rhs_pos reg2;
      prepare_symbol_rloc ();
      let pos = !symbol_rloc_ref in
      let res = f v_2 in
      res, pos) () ~finally: (fun () -> 
        pop_rhs_tbl cntr;
        symbol_rloc_ref := pos_back);
  ) arg

(* The best interface *)
let mkexp_postfix v_1 name f =
  let rhs_info = List.hd !rhs_tbl_stack in
  Op_prec.postfix name (fun (v_1, reg1) ->
    let pos_back = !symbol_rloc_ref in
    let cntr = push_rhs_tbl ("mkpat_postfix_" ^ name) in
    Spotlib.Spot.protect (fun () ->
      rhs_replay_with_override rhs_info [1, reg1];
      prepare_symbol_rloc ();
      let pos = !symbol_rloc_ref in
      let res = f v_1 in
      res, pos) () ~finally: (fun () -> 
        pop_rhs_tbl cntr;
        symbol_rloc_ref := pos_back);
  ) v_1

let mku_op f name arg = 
  let reg1 = rhs_reg 1 in
  Op_prec.prefix "~" (* CR jfuruse: quick workaround *) (fun (arg, reg) -> 
    let pos_back = !symbol_rloc_ref in
    let cntr = push_rhs_tbl ("mku_op_" ^ name) in
    Spotlib.Spot.protect (fun () ->
      add_rhs_pos reg1;
      add_rhs_pos reg;
      prepare_symbol_rloc ();
      let pos = !symbol_rloc_ref in
      let res = f name arg in
      res, pos) () ~finally: (fun () -> 
        pop_rhs_tbl cntr;
        symbol_rloc_ref := pos_back);
  ) arg

let mkuplus = mku_op mkuplus
let mkuminus = mku_op mkuminus

let build_tuple (es : ('a * Planck.Position.Region.t) Op_prec.t list) (f : 'a list -> 'a) : 'a = 
  fst (Op_prec.build (Op_prec.list "," (fun e_reg_lst -> 
    let pos_back = !symbol_rloc_ref in
    let cntr = push_rhs_tbl "build_tuple" in
    Spotlib.Spot.protect (fun () ->
      List.iter (fun (_,reg) -> add_rhs_pos reg) e_reg_lst;
      prepare_symbol_rloc ();
      let pos = !symbol_rloc_ref in
      let res = f (List.map fst e_reg_lst) in
      res, pos) () ~finally: (fun () ->
        pop_rhs_tbl cntr;
        symbol_rloc_ref := pos_back)) es))

(* /header *)

(* declarations *)
let get_CHAR = token_result (function (CHAR v) -> Result.Ok v | _ -> Result.Error "expected CHAR")
let get_FLOAT = token_result (function (FLOAT v) -> Result.Ok v | _ -> Result.Error "expected FLOAT")
let get_INFIXOP0 = token_result (function (INFIXOP0 v) -> Result.Ok v | _ -> Result.Error "expected INFIXOP0")
let get_INFIXOP1 = token_result (function (INFIXOP1 v) -> Result.Ok v | _ -> Result.Error "expected INFIXOP1")
let get_INFIXOP2 = token_result (function (INFIXOP2 v) -> Result.Ok v | _ -> Result.Error "expected INFIXOP2")
let get_INFIXOP3 = token_result (function (INFIXOP3 v) -> Result.Ok v | _ -> Result.Error "expected INFIXOP3")
let get_INFIXOP4 = token_result (function (INFIXOP4 v) -> Result.Ok v | _ -> Result.Error "expected INFIXOP4")
let get_INT = token_result (function (INT v) -> Result.Ok v | _ -> Result.Error "expected INT")
let get_INT32 = token_result (function (INT32 v) -> Result.Ok v | _ -> Result.Error "expected INT32")
let get_INT64 = token_result (function (INT64 v) -> Result.Ok v | _ -> Result.Error "expected INT64")
let get_LABEL = token_result (function (LABEL v) -> Result.Ok v | _ -> Result.Error "expected LABEL")
let get_LIDENT = token_result (function (LIDENT v) -> Result.Ok v | _ -> Result.Error "expected LIDENT")
let get_NATIVEINT = token_result (function (NATIVEINT v) -> Result.Ok v | _ -> Result.Error "expected NATIVEINT")
let get_OPTLABEL = token_result (function (OPTLABEL v) -> Result.Ok v | _ -> Result.Error "expected OPTLABEL")
let get_PREFIXOP = token_result (function (PREFIXOP v) -> Result.Ok v | _ -> Result.Error "expected PREFIXOP")
let get_STRING = token_result (function (STRING v) -> Result.Ok v | _ -> Result.Error "expected STRING")
let get_UIDENT = token_result (function (UIDENT v) -> Result.Ok v | _ -> Result.Error "expected UIDENT")
(* (Nonassoc (IN)) *)
(* (Nonassoc (below_SEMI)) *)
(* (Nonassoc (SEMI)) *)
(* (Nonassoc (LET)) *)
(* (Nonassoc (below_WITH)) *)
(* (Nonassoc (FUNCTION WITH)) *)
(* (Nonassoc (AND)) *)
(* (Nonassoc (THEN)) *)
(* (Nonassoc (ELSE)) *)
(* (Nonassoc (LESSMINUS)) *)
(* (Left (COLONEQUAL)) *)
(* (Nonassoc (AS)) *)
(* (Left (BAR)) *)
(* (Nonassoc (below_COMMA)) *)
(* (Left (COMMA)) *)
(* (Left (MINUSGREATER)) *)
(* (Left (OR BARBAR)) *)
(* (Left (AMPERSAND AMPERAMPER)) *)
(* (Nonassoc (below_EQUAL)) *)
(* (Left (INFIXOP0 EQUAL LESS GREATER)) *)
(* (Left (INFIXOP1)) *)
(* (Left (COLONCOLON)) *)
(* (Left (INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT)) *)
(* (Left (INFIXOP3 STAR)) *)
(* (Left (INFIXOP4)) *)
(* (Nonassoc (prec_unary_minus prec_unary_plus)) *)
(* (Nonassoc (prec_constant_constructor)) *)
(* (Nonassoc (prec_constr_appl)) *)
(* (Nonassoc (below_SHARP)) *)
(* (Nonassoc (SHARP)) *)
(* (Nonassoc (below_DOT)) *)
(* (Nonassoc (DOT)) *)
(* (Nonassoc
    (BACKQUOTE BANG BEGIN CHAR FALSE FLOAT INT INT32 INT64 LBRACE LBRACELESS
     LBRACKET LBRACKETBAR LIDENT LPAREN NEW NATIVEINT PREFIXOP STRING TRUE
     UIDENT)) *)
(* (Start (implementation)) *)
(* (Type Parsetree.structure (implementation)) *)
(* (Start (interface)) *)
(* (Type Parsetree.signature (interface)) *)
(* (Start (toplevel_phrase)) *)
(* (Type Parsetree.toplevel_phrase (toplevel_phrase)) *)
(* (Start (use_file)) *)
(* (Type "Parsetree.toplevel_phrase list" (use_file)) *)
(* (Start (locident)) *)
(* (Type Locident.t (locident)) *)
(* /declarations *)

(* rules *)

let check_close (close : 'a Token.Parser.t) (f : 'a -> 'b) (errf : unit -> 'b) = 
  (perform v <-- close; return (fun () -> f v)) <|> (perform take_ (* error *); return errf) 

let gen_list : 
    'a 'b. unit Token.Parser.t -> string 
      -> 'a list Token.Parser.t -> unit Token.Parser.t 
        -> unit Token.Parser.t -> string
          -> ('a list -> 'b) -> (unit -> 'b) Token.Parser.t = fun left nleft list opt right nright f -> perform
    left;
    (perform right; return (fun () -> f []))
    <|> (perform     
           v_2 <-- list;
           _v_3 <-- opt;
           (perform
              right;
              return (fun () -> f (List.rev v_2)))
           <|> (perform
                  take_; (* error *)
                  return (fun () -> unclosed nleft 1 nright 4)))

class rules = object (self)
  method implementation = rule "implementation" (fun () -> dummy
    <|> case "implementation_0" (perform

           v_1 <-- self#structure ;
           eos;

           return (fun () ->  v_1 ))

      )

  method interface = rule "interface" (fun () -> dummy
    <|> case "interface_0" (perform

           v_1 <-- self#signature ;
           eos;

           return (fun () ->  List.rev v_1 ))

      )

  method toplevel_phrase = rule "toplevel_phrase" (fun () -> dummy
    <|> case "toplevel_phrase_2" (perform (* always start with '#' *)

           v_1 <-- self#toplevel_directive ;
           token SEMISEMI;

           return (fun () ->  v_1 ))

    <|> case "toplevel_phrase_3" (perform

           eos;

           return (fun () ->  raise End_of_file ))

    <|> case "toplevel_phrase_0" (perform

           v_1 <-- self#seq_expr ;
           token SEMISEMI;

           return (fun () ->  Ptop_def[ghstrexp v_1] ))

    <|> case "toplevel_phrase_1" (perform

           v_1 <-- self#top_structure ;
           token SEMISEMI;

           return (fun () ->  Ptop_def v_1 ))

      )

  method top_structure = rule "top_structure" (fun () -> dummy
    <|> case "top_structure_0" (perform

           v_1 <-- self#structure_item ;
           v_2_opt <-- option self#top_structure ;

           return (fun () ->  
             match v_2_opt with
             | Some v_2 -> v_1 :: v_2 
             | None -> [v_1])))

  method use_file = rule "use_file" (fun () -> dummy
    <|> case "use_file_0" (perform

           v_1 <-- self#seq_expr ;
           v_2 <-- self#use_file_tail ;

           return (fun () ->  Ptop_def[ghstrexp v_1] :: v_2 ))

    <|> case "use_file_1" (perform

           v_1 <-- self#use_file_tail ;

           return (fun () ->  v_1 ))

      )

  method use_file_tail = rule "use_file_tail" (fun () -> dummy
    <|> case "use_file_tail_6" (perform

           eos;

           return (fun () ->  [] ))

    <|> case "use_file_tail_5" (perform

           v_1 <-- self#toplevel_directive ;
           v_2 <-- self#use_file_tail ;

           return (fun () ->  v_1 :: v_2 ))

    <|> case "use_file_tail_0" (perform

           token SEMISEMI;

           (perform

                  v_2 <-- self#toplevel_directive ;
                  v_3 <-- self#use_file_tail ;

                  return (fun () ->  v_2 :: v_3 ))

           <|> (perform

                  eos;

                return (fun () ->  [] ))

           <|> (perform                                

                  v_2 <-- self#seq_expr ;
                  v_3 <-- self#use_file_tail ;

                  return (fun () ->  Ptop_def[ghstrexp v_2] :: v_3 ))

           <|> (perform

                  v_2 <-- self#structure_item ;
                  v_3 <-- self#use_file_tail ;

                  return (fun () ->  Ptop_def[v_2] :: v_3 ))
    )


    <|> case "use_file_tail_4" (perform

           v_1 <-- self#structure_item ;
           v_2 <-- self#use_file_tail ;

           return (fun () ->  Ptop_def[v_1] :: v_2 ))

      )

  method module_expr = leftrec "module_expr" self#module_expr_nonleftrec self#module_expr_leftrec

  method module_expr_nonleftrec = (dummy
    <|> case "module_expr_nonleftrec_0" (perform

           token FUNCTOR;
           token LPAREN;
           v_3 <-- get_UIDENT;
           token COLON;
           v_5 <-- self#module_type ;
           token RPAREN;
           token MINUSGREATER;
           v_8 <-- self#module_expr ;

           return (fun () ->  mkmod(Pmod_functor(v_3, v_5, v_8)) ))

    <|> case "module_expr_nonleftrec_7" (perform

           token STRUCT;
           v_2 <-- self#structure ;
           check_close
             (token END)
             (fun () ->  mkmod(Pmod_structure(v_2)))
             (fun () ->  unclosed "struct" 1 "end" 3 ))

    <|> case "module_expr_nonleftrec_9" (perform

           v_1 <-- self#mod_longident ;

           return (fun () ->  mkmod(Pmod_ident v_1) ))

    <|> case "module_expr_nonleftrec_1" (perform

           token LPAREN;

           (perform                                         
              token VAL;
              v_3 <-- self#expr ;
              token COLON;
              check_close
                (perform
                   v_5 <-- self#package_type ;
                   token RPAREN;
                   return v_5)
                (fun v_5 ->  mkmod(Pmod_unpack(v_3, v_5)))
                (fun () ->  unclosed "(" 1 ")" 5 ))

           <|> (perform
                v_2 <-- self#module_expr ;
                v_4 <-- option (perform
                             token COLON;
                             self#module_type) ;
                check_close
                  (token RPAREN)
                  (fun () ->  
                    match v_4 with
                    | Some v_4 -> mkmod(Pmod_constraint(v_2, v_4))
                    | None -> v_2)
                  (fun () ->  unclosed "(" 1 ")" 5 )))

  )

  method module_expr_leftrec v_1 = (dummy
    <|> case "module_expr_leftrec_0" (perform

           token LPAREN;
           v_3 <-- self#module_expr ;
           check_close 
             (token RPAREN)
             (fun () ->  mkmod(Pmod_apply(v_1, v_3)))
             (fun () ->  unclosed "(" 2 ")" 4))

  )

  method structure = rule "structure" (fun () -> dummy
    <|> case "structure_0" (perform

           v_1 <-- self#seq_expr ;
           v_2 <-- self#structure_tail ;

           return (fun () ->  ghstrexp v_1 :: v_2 ))

    <|> case "structure_1" (perform

           v_1 <-- self#structure_tail ;

           return (fun () ->  v_1 ))

      )

  method structure_tail = rule "structure_tail" (fun () -> dummy
    <|> case "structure_tail_0" (perform

           token SEMISEMI;

           (perform                                 
              v_2 <-- self#seq_expr ;
              v_3 <-- self#structure_tail ;

              return (fun () ->  ghstrexp v_2 :: v_3 ))
             
           <|> (perform
                  v_2 <-- self#structure_item ;
                  v_3 <-- self#structure_tail ;

                  return (fun () ->  v_2 :: v_3 ))

           <|> (perform
                  
                  return (fun () ->  [] )))

    <|> case "structure_tail_2" (perform

           v_1 <-- self#structure_item ;
           v_2 <-- self#structure_tail ;

           return (fun () ->  v_1 :: v_2 ))

    <|> case "structure_tail_4" (perform

           (* empty *)

           return (fun () ->  [] ))

      )

  method structure_item = rule "structure_item" (fun () -> dummy
    <|> case "structure_item_0" (perform

           token EXTERNAL;
           v_2 <-- self#val_ident ;
           token COLON;
           v_4 <-- self#core_type ;
           token EQUAL;
           v_6 <-- self#primitive_declaration ;

           return (fun () ->  mkstr(Pstr_primitive(v_2, {pval_type = v_4; pval_prim = v_6})) ))

    <|> case "structure_item_9" (perform

           token INCLUDE;
           v_2 <-- self#module_expr ;

           return (fun () ->  mkstr(Pstr_include v_2) ))

    <|> case "structure_item_10" (perform

           token OPEN;
           v_2 <-- self#mod_longident ;

           return (fun () ->  mkstr(Pstr_open v_2) ))

    <|> case "structure_item_11" (perform

           token TYPE;
           v_2 <-- self#type_declarations ;

           return (fun () ->  mkstr(Pstr_type(List.rev v_2)) ))

    <|> case "structure_item_5" (perform

           token LET;
           v_2 <-- self#rec_flag ;
           v_3 <-- self#let_bindings ;

           return (fun () ->  match v_3 with
          [{ppat_desc = Ppat_any; _ }, exp] -> mkstr(Pstr_eval exp)
        | _ -> mkstr(Pstr_value(v_2, List.rev v_3)) ))

    <|> case "structure_item_2" ( (perform

             token EXCEPTION;
             v_2 <-- get_UIDENT;
             token EQUAL;
             v_4 <-- self#constr_longident ;
     
             return (fun () ->  mkstr(Pstr_exn_rebind(v_2, v_4)) ))

           <!> (perform

             token EXCEPTION;
             v_2 <-- get_UIDENT;
             v_3 <-- self#constructor_arguments ;
     
             return (fun () ->  mkstr(Pstr_exception(v_2, v_3)) ))
        )

    <|> case "structure_item_3" ( (perform

             token CLASS;
             token TYPE;
             v_3 <-- self#class_type_declarations ;
  
             return (fun () ->  mkstr(Pstr_class_type (List.rev v_3)) ))

           <!> (perform

             token CLASS;
             v_2 <-- self#class_declarations ;
  
             return (fun () ->  mkstr(Pstr_class (List.rev v_2)) ))
      )

    <|> case "structure_item_1" (perform

           token MODULE;
           token TYPE;
           v_3 <-- self#ident ;
           token EQUAL;
           v_5 <-- self#module_type ;

           return (fun () ->  mkstr(Pstr_modtype(v_3, v_5)) ))

    <!> case "structure_item_6" (perform

           token MODULE;
           token REC;
           v_3 <-- self#module_rec_bindings ;

           return (fun () ->  mkstr(Pstr_recmodule(List.rev v_3)) ))

    <!> case "structure_item_7" (perform

           token MODULE;
           v_2 <-- get_UIDENT;
           v_3 <-- self#module_binding ;

           return (fun () ->  mkstr(Pstr_module(v_2, v_3)) ))

      )

  method module_binding = rule "module_binding" (fun () -> dummy
    <|> case "module_binding_0" (perform

           token LPAREN;
           v_2 <-- get_UIDENT;
           token COLON;
           v_4 <-- self#module_type ;
           token RPAREN;
           v_6 <-- self#module_binding ;

           return (fun () ->  mkmod(Pmod_functor(v_2, v_4, v_6)) ))

    <|> case "module_binding_1" (perform

           token COLON;
           v_2 <-- self#module_type ;
           token EQUAL;
           v_4 <-- self#module_expr ;

           return (fun () ->  mkmod(Pmod_constraint(v_4, v_2)) ))

    <|> case "module_binding_2" (perform

           token EQUAL;
           v_2 <-- self#module_expr ;

           return (fun () ->  v_2 ))

      )

  method module_rec_bindings = leftrec "module_rec_bindings" self#module_rec_bindings_nonleftrec self#module_rec_bindings_leftrec

  method module_rec_bindings_nonleftrec = (dummy
    <|> case "module_rec_bindings_nonleftrec_0" (perform

           v_1 <-- self#module_rec_binding ;

           return (fun () ->  [v_1] ))

      )

  method module_rec_bindings_leftrec v_1 = (dummy
    <|> case "module_rec_bindings_leftrec_0" (perform

           token AND;
           v_3 <-- self#module_rec_binding ;

           return (fun () ->  v_3 :: v_1 ))

      )

  method module_rec_binding = rule "module_rec_binding" (fun () -> dummy
    <|> case "module_rec_binding_0" (perform

           v_1 <-- get_UIDENT;
           token COLON;
           v_3 <-- self#module_type ;
           token EQUAL;
           v_5 <-- self#module_expr ;

           return (fun () ->  (v_1, v_3, v_5) ))

      )

  method module_type = leftrec "module_type" self#module_type_nonleftrec self#module_type_leftrec

  method module_type_nonleftrec = (dummy
    <|> case "module_type_nonleftrec_0" (perform

           token FUNCTOR;
           token LPAREN;
           v_3 <-- get_UIDENT;
           token COLON;
           v_5 <-- self#module_type ;
           token RPAREN;
           token MINUSGREATER;
           v_8 <-- self#module_type ;
           (* %prec below_WITH *)

           return (fun () ->  mkmty(Pmty_functor(v_3, v_5, v_8)) ))

    <|> case "module_type_nonleftrec_1" (perform

           token MODULE;
           token TYPE;
           token OF;
           v_4 <-- self#module_expr ;

           return (fun () ->  mkmty(Pmty_typeof v_4) ))

    <|> case "module_type_nonleftrec_2" (perform

           token LPAREN;
           v_2 <-- self#module_type ;
           check_close
             (token RPAREN)
             (fun () ->  v_2 )
             (fun () ->  unclosed "(" 1 ")" 3 ))

    <|> case "module_type_nonleftrec_4" (perform

           token SIG;
           v_2 <-- self#signature ;
           check_close
             (token END)
             (fun () ->  mkmty(Pmty_signature(List.rev v_2)))
             (fun () ->  unclosed "sig" 1 "end" 3 ))

    <|> case "module_type_nonleftrec_6" (perform

           v_1 <-- self#mty_longident ;

           return (fun () ->  mkmty(Pmty_ident v_1) ))

      )

  method module_type_leftrec v_1 = (dummy
    <|> case "module_type_leftrec_0" (perform

           token WITH;
           v_3 <-- self#with_constraints ;

           return (fun () ->  mkmty(Pmty_with(v_1, List.rev v_3)) ))

      )

  method signature = leftrec "signature" self#signature_nonleftrec self#signature_leftrec

  method signature_nonleftrec = (dummy
    <|> case "signature_nonleftrec_0" (perform

           (* empty *)

           return (fun () ->  [] ))

      )

(* TYPPPICAL EXAMPLE OF INEFFICIENCY!
  method signature_leftrec v_1 = (dummy
    <|> case "signature_leftrec_0" (perform

           v_2 <-- self#signature_item ;
           token SEMISEMI;

           return (fun () ->  v_2 :: v_1 ))

    <!> case "signature_leftrec_1" (perform

           v_2 <-- self#signature_item ;

           return (fun () ->  v_2 :: v_1 ))

      )
*)


  method signature_leftrec v_1 = (dummy
    <|> case "signature_leftrec_0" (perform

           v_2 <-- self#signature_item ;
           ignore (option (token SEMISEMI));

           return (fun () ->  v_2 :: v_1 ))
  )


  method signature_item = rule "signature_item" (fun () -> dummy
    <|> case "signature_item_0" (perform

           token EXTERNAL;
           v_2 <-- self#val_ident ;
           token COLON;
           v_4 <-- self#core_type ;
           token EQUAL;
           v_6 <-- self#primitive_declaration ;

           return (fun () ->  mksig(Psig_value(v_2, {pval_type = v_4; pval_prim = v_6})) ))

    <|> case "signature_item_2" (perform

           token VAL;
           v_2 <-- self#val_ident ;
           token COLON;
           v_4 <-- self#core_type ;

           return (fun () ->  mksig(Psig_value(v_2, {pval_type = v_4; pval_prim = []})) ))

    <|> case "signature_item_4" (perform

           token EXCEPTION;
           v_2 <-- get_UIDENT;
           v_3 <-- self#constructor_arguments ;

           return (fun () ->  mksig(Psig_exception(v_2, v_3)) ))

    <|> case "signature_item_9" (perform

           token INCLUDE;
           v_2 <-- self#module_type ;

           return (fun () ->  mksig(Psig_include v_2) ))

    <|> case "signature_item_10" (perform

           token OPEN;
           v_2 <-- self#mod_longident ;

           return (fun () ->  mksig(Psig_open v_2) ))

    <|> case "signature_item_11" (perform

           token TYPE;
           v_2 <-- self#type_declarations ;

           return (fun () ->  mksig(Psig_type(List.rev v_2)) ))

    <|> case "signature_item_1" (perform

           token MODULE;
           token TYPE;
           v_3 <-- self#ident ;
           token EQUAL;
           v_5 <-- self#module_type ;

           return (fun () ->  mksig(Psig_modtype(v_3, Pmodtype_manifest v_5)) ))

    <!> case "signature_item_6" (perform

           token MODULE;
           token TYPE;
           v_3 <-- self#ident ;

           return (fun () ->  mksig(Psig_modtype(v_3, Pmodtype_abstract)) ))

    <!> case "signature_item_5" (perform

           token MODULE;
           token REC;
           v_3 <-- self#module_rec_declarations ;

           return (fun () ->  mksig(Psig_recmodule(List.rev v_3)) ))

    <!> case "signature_item_7" (perform

           token MODULE;
           v_2 <-- get_UIDENT;
           v_3 <-- self#module_declaration ;

           return (fun () ->  mksig(Psig_module(v_2, v_3)) ))

    <!> case "signature_item_3" (perform

           token CLASS;
           token TYPE;
           v_3 <-- self#class_type_declarations ;

           return (fun () ->  mksig(Psig_class_type (List.rev v_3)) ))

    <!> case "signature_item_8" (perform

           token CLASS;
           v_2 <-- self#class_descriptions ;

           return (fun () ->  mksig(Psig_class (List.rev v_2)) ))

      )

  method module_declaration = rule "module_declaration" (fun () -> dummy
    <|> case "module_declaration_0" (perform

           token LPAREN;
           v_2 <-- get_UIDENT;
           token COLON;
           v_4 <-- self#module_type ;
           token RPAREN;
           v_6 <-- self#module_declaration ;

           return (fun () ->  mkmty(Pmty_functor(v_2, v_4, v_6)) ))

    <|> case "module_declaration_1" (perform

           token COLON;
           v_2 <-- self#module_type ;

           return (fun () ->  v_2 ))

      )

  method module_rec_declarations = leftrec "module_rec_declarations" self#module_rec_declarations_nonleftrec self#module_rec_declarations_leftrec

  method module_rec_declarations_nonleftrec = (dummy
    <|> case "module_rec_declarations_nonleftrec_0" (perform

           v_1 <-- self#module_rec_declaration ;

           return (fun () ->  [v_1] ))

      )

  method module_rec_declarations_leftrec v_1 = (dummy
    <|> case "module_rec_declarations_leftrec_0" (perform

           token AND;
           v_3 <-- self#module_rec_declaration ;

           return (fun () ->  v_3 :: v_1 ))

      )

  method module_rec_declaration = rule "module_rec_declaration" (fun () -> dummy
    <|> case "module_rec_declaration_0" (perform

           v_1 <-- get_UIDENT;
           token COLON;
           v_3 <-- self#module_type ;

           return (fun () ->  (v_1, v_3) ))

      )

  method class_declarations = leftrec "class_declarations" self#class_declarations_nonleftrec self#class_declarations_leftrec

  method class_declarations_nonleftrec = (dummy
    <|> case "class_declarations_nonleftrec_0" (perform

           v_1 <-- self#class_declaration ;

           return (fun () ->  [v_1] ))

      )

  method class_declarations_leftrec v_1 = (dummy
    <|> case "class_declarations_leftrec_0" (perform

           token AND;
           v_3 <-- self#class_declaration ;

           return (fun () ->  v_3 :: v_1 ))

      )

  method class_declaration = rule "class_declaration" (fun () -> dummy
    <|> case "class_declaration_0" (perform

           v_1 <-- self#virtual_flag ;
           v_2 <-- self#class_type_parameters ;
           v_3 <-- get_LIDENT;
           v_4 <-- self#class_fun_binding ;

           return (fun () ->  let params, variance = List.split (fst v_2) in
        {pci_virt = v_1; pci_params = params, snd v_2;
         pci_name = v_3; pci_expr = v_4; pci_variance = variance;
         pci_loc = symbol_rloc ()} ))

      )

  method class_fun_binding = rule "class_fun_binding" (fun () -> dummy
    <|> case "class_fun_binding_0" (perform

           token COLON;
           v_2 <-- self#class_type ;
           token EQUAL;
           v_4 <-- self#class_expr ;

           return (fun () ->  mkclass(Pcl_constraint(v_4, v_2)) ))

    <|> case "class_fun_binding_1" (perform

           token EQUAL;
           v_2 <-- self#class_expr ;

           return (fun () ->  v_2 ))

    <|> case "class_fun_binding_2" (perform

           v_1 <-- self#labeled_simple_pattern ;
           v_2 <-- self#class_fun_binding ;

           return (fun () ->  let (l,o,p) = v_1 in mkclass(Pcl_fun(l, o, p, v_2)) ))

      )

  method class_type_parameters = rule "class_type_parameters" (fun () -> dummy
    <|> case "class_type_parameters_0" (perform

           token LBRACKET;
           v_2 <-- self#type_parameter_list ;
           token RBRACKET;

           return (fun () ->  List.rev v_2, symbol_rloc () ))

    <|> case "class_type_parameters_1" (perform

           (* empty *)

           return (fun () ->  [], symbol_gloc () ))

      )

  method class_fun_def = rule "class_fun_def" (fun () -> dummy
    <|> case "class_fun_def_0" (perform

           v_1 <-- self#labeled_simple_pattern ;
           token MINUSGREATER;
           v_3 <-- self#class_expr ;

           return (fun () ->  let (l,o,p) = v_1 in mkclass(Pcl_fun(l, o, p, v_3)) ))

    <!> case "class_fun_def_1" (perform

           v_1 <-- self#labeled_simple_pattern ;
           v_2 <-- self#class_fun_def ;

           return (fun () ->  let (l,o,p) = v_1 in mkclass(Pcl_fun(l, o, p, v_2)) ))

      )

  method class_expr = rule "class_expr" (fun () -> dummy
    <|> case "class_expr_0" (perform

           token LET;
           v_2 <-- self#rec_flag ;
           v_3 <-- self#let_bindings ;
           token IN;
           v_5 <-- self#class_expr ;

           return (fun () ->  mkclass(Pcl_let (v_2, List.rev v_3, v_5)) ))

    <|> case "class_expr_1" (perform

           token FUN;
           v_2 <-- self#class_fun_def ;

           return (fun () ->  v_2 ))

    <|> case "class_expr_2" (perform

           v_1 <-- self#class_simple_expr ;
           v_2 <-- option (self#simple_labeled_expr_list) ;

           return (fun () ->  
             match v_2 with
             | Some v_2 -> mkclass(Pcl_apply(v_1, List.rev v_2))
             | None -> v_1))

  )

  method class_simple_expr = rule "class_simple_expr" (fun () -> dummy
    <|> case "class_simple_expr_2" (perform

           token LBRACKET;
           v_2 <-- self#core_type_comma_list ;
           token RBRACKET;
           v_4 <-- self#class_longident ;

           return (fun () ->  mkclass(Pcl_constr(v_4, List.rev v_2)) ))

    <|> case "class_simple_expr_5" (perform

           token OBJECT;
           v_2 <-- self#class_structure ;
           check_close 
             (token END)
             (fun () ->  mkclass(Pcl_structure(v_2)))
             (fun () ->  unclosed "object" 1 "end" 3))

    <|> case "class_simple_expr_0" (perform

           token LPAREN;
           v_2 <-- self#class_expr ;
           token COLON;
           v_4 <-- self#class_type ;
           check_close
             (token RPAREN)
             (fun () ->  mkclass(Pcl_constraint(v_2, v_4)))
             (fun () ->  unclosed "(" 1 ")" 5 ))

    <!> case "class_simple_expr_3" (perform

           token LPAREN;
           v_2 <-- self#class_expr ;
           check_close 
             (token RPAREN)
             (fun () ->  v_2 )
             (fun () ->  unclosed "(" 1 ")" 3 ))

    <!> case "class_simple_expr_7" (perform

           v_1 <-- self#class_longident ;

           return (fun () ->  mkclass(Pcl_constr(v_1, [])) ))

      )

  method class_structure = rule "class_structure" (fun () -> dummy
    <|> case "class_structure_0" (perform

           v_1 <-- self#class_self_pattern ;
           v_2 <-- self#class_fields ;

           return (fun () ->  v_1, List.rev v_2 ))

      )

  method class_self_pattern = rule "class_self_pattern" (fun () -> dummy
    <|> case "class_self_pattern_0" (perform

           token LPAREN;
           v_2 <-- self#pattern ;
           token COLON;
           v_4 <-- self#core_type ;
           token RPAREN;

           return (fun () ->  mkpat(Ppat_constraint(v_2, v_4)) ))

    <!> case "class_self_pattern_1" (perform

           token LPAREN;
           v_2 <-- self#pattern ;
           token RPAREN;

           return (fun () ->  reloc_pat v_2 ))

    <!> case "class_self_pattern_2" (perform

           (* empty *)

           return (fun () ->  ghpat(Ppat_any) ))

      )

  method class_fields = leftrec "class_fields" self#class_fields_nonleftrec self#class_fields_leftrec

  method class_fields_nonleftrec = (dummy
    <|> case "class_fields_nonleftrec_0" (perform

           (* empty *)

           return (fun () ->  [] ))

      )

  method class_fields_leftrec v_1 = (dummy
    <|> case "class_fields_leftrec_0" (perform

           token INHERIT;
           v_3 <-- self#override_flag ;
           v_4 <-- self#class_expr ;
           v_5 <-- self#parent_binder ;

           return (fun () ->  Pcf_inher (v_3, v_4, v_5) :: v_1 ))

    <|> case "class_fields_leftrec_1" (perform

           token CONSTRAINT;
           v_3 <-- self#constrain ;

           return (fun () ->  Pcf_cstr v_3 :: v_1 ))

    <|> case "class_fields_leftrec_2" (perform

           token INITIALIZER;
           v_3 <-- self#seq_expr ;

           return (fun () ->  Pcf_init v_3 :: v_1 ))

    <|> case "class_fields_leftrec_3" (perform

           token VAL;
           v_3 <-- self#value ;

           return (fun () ->  Pcf_val v_3 :: v_1 ))

    <!> case "class_fields_leftrec_4" (perform

           token VAL;
           v_3 <-- self#virtual_value ;

           return (fun () ->  Pcf_valvirt v_3 :: v_1 ))

    <!> case "class_fields_leftrec_5" (perform

           v_2 <-- self#concrete_method ;

           return (fun () ->  Pcf_meth v_2 :: v_1 ))

    <!> case "class_fields_leftrec_6" (perform

           v_2 <-- self#virtual_method ;

           return (fun () ->  Pcf_virt v_2 :: v_1 ))

      )

  method parent_binder = rule "parent_binder" (fun () -> dummy
    <|> case "parent_binder_0" (perform

           token AS;
           v_2 <-- get_LIDENT;

           return (fun () ->  Some v_2 ))

    <|> case "parent_binder_1" (perform

           (* empty *)

           return (fun () ->  None ))

      )

  method virtual_value = rule "virtual_value" (fun () -> dummy
    <|> case "virtual_value_0" (perform

           v_1 <-- self#override_flag ;
           token MUTABLE;
           token VIRTUAL;
           v_4 <-- self#label ;
           token COLON;
           v_6 <-- self#core_type ;

           return (fun () ->  if v_1 = Override then syntax_error ();
        v_4, Mutable, v_6, symbol_rloc () ))

    <|> case "virtual_value_1" (perform

           token VIRTUAL;
           v_2 <-- self#mutable_flag ;
           v_3 <-- self#label ;
           token COLON;
           v_5 <-- self#core_type ;

           return (fun () ->  v_3, v_2, v_5, symbol_rloc () ))

      )

  method value = rule "value" (fun () -> dummy
    <|> case "value_0" (perform

           v_1 <-- self#override_flag ;
           v_2 <-- self#mutable_flag ;
           v_3 <-- self#label ;
           v_4 <-- option self#type_constraint ;
           token EQUAL;
           v_6 <-- self#seq_expr ;

           return (fun () -> 
             match v_4 with
             | Some v_4 -> v_3, v_2, v_1, (let (t, t') = v_4 in ghexp(Pexp_constraint(v_6, t, t'))), symbol_rloc () 
             | None -> v_3, v_2, v_1, v_6, symbol_rloc () ))

      )

  method virtual_method = rule "virtual_method" (fun () -> dummy
    <|> case "virtual_method_0" (perform

           token METHOD;
           v_2 <-- self#override_flag ;
           token PRIVATE;
           token VIRTUAL;
           v_5 <-- self#label ;
           token COLON;
           v_7 <-- self#poly_type ;

           return (fun () ->  if v_2 = Override then syntax_error ();
        v_5, Private, v_7, symbol_rloc () ))

    <!> case "virtual_method_1" (perform

           token METHOD;
           v_2 <-- self#override_flag ;
           token VIRTUAL;
           v_4 <-- self#private_flag ;
           v_5 <-- self#label ;
           token COLON;
           v_7 <-- self#poly_type ;

           return (fun () ->  if v_2 = Override then syntax_error ();
        v_5, v_4, v_7, symbol_rloc () ))

      )

  method concrete_method = rule "concrete_method" (fun () -> dummy
    <|> case "concrete_method_0" (perform

           token METHOD;
           v_2 <-- self#override_flag ;
           v_3 <-- self#private_flag ;
           v_4 <-- self#label ;
           token COLON;
           v_6 <-- self#poly_type ;
           token EQUAL;
           v_8 <-- self#seq_expr ;

           return (fun () ->  v_4, v_3, v_2, ghexp(Pexp_poly(v_8,Some v_6)), symbol_rloc () ))

    <!> case "concrete_method_1" (perform

           token METHOD;
           v_2 <-- self#override_flag ;
           v_3 <-- self#private_flag ;
           v_4 <-- self#label ;
           v_5 <-- self#strict_binding ;

           return (fun () ->  v_4, v_3, v_2, ghexp(Pexp_poly (v_5, None)), symbol_rloc () ))

      )

  method class_type = rule "class_type" (fun () -> dummy
    <|> case "class_type_0" (perform

           token QUESTION;
           v_2 <-- get_LIDENT;
           token COLON;
           v_4 <-- self#simple_core_type_or_tuple ;
           token MINUSGREATER;
           v_6 <-- self#class_type ;

           return (fun () ->  mkcty(Pcty_fun("?" ^ v_2 ,
                       {ptyp_desc =
                        Ptyp_constr(Ldot (Lident "*predef*", "option"), [v_4]);
                        ptyp_loc = v_4.ptyp_loc},
                       v_6)) ))

    <|> case "class_type_2" (perform

           v_1 <-- get_OPTLABEL;
           v_2 <-- self#simple_core_type_or_tuple ;
           token MINUSGREATER;
           v_4 <-- self#class_type ;

           return (fun () ->  mkcty(Pcty_fun("?" ^ v_1 ,
                       {ptyp_desc =
                        Ptyp_constr(Ldot (Lident "*predef*", "option"), [v_2]);
                        ptyp_loc = v_2.ptyp_loc},
                       v_4)) ))

    <|> case "class_type_1" (perform

           v_1 <-- get_LIDENT;
           token COLON;
           v_3 <-- self#simple_core_type_or_tuple ;
           token MINUSGREATER;
           v_5 <-- self#class_type ;

           return (fun () ->  mkcty(Pcty_fun(v_1, v_3, v_5)) ))

    <!> case "class_type_3" (perform

           v_1 <-- self#simple_core_type_or_tuple ;
           token MINUSGREATER;
           v_3 <-- self#class_type ;

           return (fun () ->  mkcty(Pcty_fun("", v_1, v_3)) ))

    <!> case "class_type_4" (perform

           v_1 <-- self#class_signature ;

           return (fun () ->  v_1 ))

      )

  method class_signature = rule "class_signature" (fun () -> dummy
    <|> case "class_signature_0" (perform

           token LBRACKET;
           v_2 <-- self#core_type_comma_list ;
           token RBRACKET;
           v_4 <-- self#clty_longident ;

           return (fun () ->  mkcty(Pcty_constr (v_4, List.rev v_2)) ))

    <|> case "class_signature_1" (perform

           token OBJECT;
           v_2 <-- self#class_sig_body ;
           check_close
             (token END)
             (fun () ->  mkcty(Pcty_signature v_2))
             (fun () ->  unclosed "object" 1 "end" 3))

    <|> case "class_signature_3" (perform

           v_1 <-- self#clty_longident ;

           return (fun () ->  mkcty(Pcty_constr (v_1, [])) ))

      )

  method class_sig_body = rule "class_sig_body" (fun () -> dummy
    <|> case "class_sig_body_0" (perform

           v_1 <-- self#class_self_type ;
           v_2 <-- self#class_sig_fields ;

           return (fun () ->  v_1, List.rev v_2 ))

      )

  method class_self_type = rule "class_self_type" (fun () -> dummy
    <|> case "class_self_type_0" (perform

           token LPAREN;
           v_2 <-- self#core_type ;
           token RPAREN;

           return (fun () ->  v_2 ))

    <|> case "class_self_type_1" (perform

           (* empty *)

           return (fun () ->  mktyp(Ptyp_any) ))

      )

  method class_sig_fields = leftrec "class_sig_fields" self#class_sig_fields_nonleftrec self#class_sig_fields_leftrec

  method class_sig_fields_nonleftrec = (dummy
    <|> case "class_sig_fields_nonleftrec_0" (perform

           (* empty *)

           return (fun () ->  [] ))

      )

  method class_sig_fields_leftrec v_1 = (dummy
    <|> case "class_sig_fields_leftrec_0" (perform

           token CONSTRAINT;
           v_3 <-- self#constrain ;

           return (fun () ->  Pctf_cstr  v_3 :: v_1 ))

    <|> case "class_sig_fields_leftrec_1" (perform

           token INHERIT;
           v_3 <-- self#class_signature ;

           return (fun () ->  Pctf_inher v_3 :: v_1 ))

    <|> case "class_sig_fields_leftrec_2" (perform

           token VAL;
           v_3 <-- self#value_type ;

           return (fun () ->  Pctf_val   v_3 :: v_1 ))

    <|> case "class_sig_fields_leftrec_3" (perform

           v_2 <-- self#method_type ;

           return (fun () ->  Pctf_meth  v_2 :: v_1 ))

    <!> case "class_sig_fields_leftrec_4" (perform

           v_2 <-- self#virtual_method_type ;

           return (fun () ->  Pctf_virt  v_2 :: v_1 ))

      )

  method value_type = rule "value_type" (fun () -> dummy
    <|> case "value_type_0" (perform

           token MUTABLE;
           v_2 <-- self#virtual_flag ;
           v_3 <-- self#label ;
           token COLON;
           v_5 <-- self#core_type ;

           return (fun () ->  v_3, Mutable, v_2, v_5, symbol_rloc () ))

    <|> case "value_type_1" (perform

           token VIRTUAL;
           v_2 <-- self#mutable_flag ;
           v_3 <-- self#label ;
           token COLON;
           v_5 <-- self#core_type ;

           return (fun () ->  v_3, v_2, Virtual, v_5, symbol_rloc () ))

    <|> case "value_type_2" (perform

           v_1 <-- self#label ;
           token COLON;
           v_3 <-- self#core_type ;

           return (fun () ->  v_1, Immutable, Concrete, v_3, symbol_rloc () ))

      )

  method method_type = rule "method_type" (fun () -> dummy
    <|> case "method_type_0" (perform

           token METHOD;
           v_2 <-- self#private_flag ;
           v_3 <-- self#label ;
           token COLON;
           v_5 <-- self#poly_type ;

           return (fun () ->  v_3, v_2, v_5, symbol_rloc () ))

      )

  method virtual_method_type = rule "virtual_method_type" (fun () -> dummy
    <|> case "virtual_method_type_0" (perform

           token METHOD;
           token PRIVATE;
           token VIRTUAL;
           v_4 <-- self#label ;
           token COLON;
           v_6 <-- self#poly_type ;

           return (fun () ->  v_4, Private, v_6, symbol_rloc () ))

    <!> case "virtual_method_type_1" (perform

           token METHOD;
           token VIRTUAL;
           v_3 <-- self#private_flag ;
           v_4 <-- self#label ;
           token COLON;
           v_6 <-- self#poly_type ;

           return (fun () ->  v_4, v_3, v_6, symbol_rloc () ))

      )

  method constrain = rule "constrain" (fun () -> dummy
    <|> case "constrain_0" (perform

           v_1 <-- self#core_type ;
           token EQUAL;
           v_3 <-- self#core_type ;

           return (fun () ->  v_1, v_3, symbol_rloc () ))

      )

  method class_descriptions = leftrec "class_descriptions" self#class_descriptions_nonleftrec self#class_descriptions_leftrec

  method class_descriptions_nonleftrec = (dummy
    <|> case "class_descriptions_nonleftrec_0" (perform

           v_1 <-- self#class_description ;

           return (fun () ->  [v_1] ))

      )

  method class_descriptions_leftrec v_1 = (dummy
    <|> case "class_descriptions_leftrec_0" (perform

           token AND;
           v_3 <-- self#class_description ;

           return (fun () ->  v_3 :: v_1 ))

      )

  method class_description = rule "class_description" (fun () -> dummy
    <|> case "class_description_0" (perform

           v_1 <-- self#virtual_flag ;
           v_2 <-- self#class_type_parameters ;
           v_3 <-- get_LIDENT;
           token COLON;
           v_5 <-- self#class_type ;

           return (fun () ->  let params, variance = List.split (fst v_2) in
        {pci_virt = v_1; pci_params = params, snd v_2;
         pci_name = v_3; pci_expr = v_5; pci_variance = variance;
         pci_loc = symbol_rloc ()} ))

      )

  method class_type_declarations = leftrec "class_type_declarations" self#class_type_declarations_nonleftrec self#class_type_declarations_leftrec

  method class_type_declarations_nonleftrec = (dummy
    <|> case "class_type_declarations_nonleftrec_0" (perform

           v_1 <-- self#class_type_declaration ;

           return (fun () ->  [v_1] ))

      )

  method class_type_declarations_leftrec v_1 = (dummy
    <|> case "class_type_declarations_leftrec_0" (perform

           token AND;
           v_3 <-- self#class_type_declaration ;

           return (fun () ->  v_3 :: v_1 ))

      )

  method class_type_declaration = rule "class_type_declaration" (fun () -> dummy
    <|> case "class_type_declaration_0" (perform

           v_1 <-- self#virtual_flag ;
           v_2 <-- self#class_type_parameters ;
           v_3 <-- get_LIDENT;
           token EQUAL;
           v_5 <-- self#class_signature ;

           return (fun () ->  let params, variance = List.split (fst v_2) in
        {pci_virt = v_1; pci_params = params, snd v_2;
         pci_name = v_3; pci_expr = v_5; pci_variance = variance;
         pci_loc = symbol_rloc ()} ))

      )

  method seq_expr = rule "seq_expr" (fun () -> dummy
    <|> case "seq_expr_0" (perform

           v_1 <-- self#expr ;
           (perform 
              token SEMI;
              v_3 <-- option self#seq_expr;
              
              return (fun () ->  
                match v_3 with
                | Some v_3 -> mkexp(Pexp_sequence(v_1, v_3))
                | None -> reloc_exp v_1)
              
           ) <|>
              (* %prec below_SEMI *)  
              return (fun () ->  v_1 ))
    )

  method labeled_simple_pattern = rule "labeled_simple_pattern" (fun () -> dummy
    <|> case "labeled_simple_pattern_7" (perform

           v_1 <-- self#simple_pattern ;

           return (fun () ->  ("", None, v_1) ))

    <|> case "labeled_simple_pattern_3" (perform

           v_1 <-- get_LABEL;
           v_2 <-- self#simple_pattern ;

           return (fun () ->  (v_1, None, v_2) ))

    <|> case "labeled_simple_pattern_2" (perform

           token TILDE;
           token LPAREN;
           v_3 <-- self#label_let_pattern ;
           token RPAREN;

           return (fun () ->  (fst v_3, None, snd v_3) ))

    <!> case "labeled_simple_pattern_6" (perform

           token TILDE;
           v_2 <-- self#label_var ;

           return (fun () ->  (fst v_2, None, snd v_2) ))

    <!> case "labeled_simple_pattern_0" (perform

           v_1 <-- get_OPTLABEL;
           token LPAREN;
           v_3 <-- self#let_pattern ;
           v_4 <-- self#opt_default ;
           token RPAREN;

           return (fun () ->  ("?" ^ v_1, v_4, v_3) ))

    <!> case "labeled_simple_pattern_4" (perform

           v_1 <-- get_OPTLABEL;
           v_2 <-- self#pattern_var ;

           return (fun () ->  ("?" ^ v_1, None, v_2) ))

    <!> case "labeled_simple_pattern_1" (perform

           token QUESTION;
           token LPAREN;
           v_3 <-- self#label_let_pattern ;
           v_4 <-- self#opt_default ;
           token RPAREN;

           return (fun () ->  ("?" ^ fst v_3, v_4, snd v_3) ))

    <!> case "labeled_simple_pattern_5" (perform

           token QUESTION;
           v_2 <-- self#label_var ;

           return (fun () ->  ("?" ^ fst v_2, None, snd v_2) ))

      )

  method pattern_var = rule "pattern_var" (fun () -> dummy
    <|> case "pattern_var_0" (perform

           v_1 <-- get_LIDENT;

           return (fun () ->  mkpat(Ppat_var v_1) ))

    <|> case "pattern_var_1" (perform

           token UNDERSCORE;

           return (fun () ->  mkpat Ppat_any ))

      )

  method opt_default = rule "opt_default" (fun () -> dummy
    <|> case "opt_default_0" (perform

           token EQUAL;
           v_2 <-- self#seq_expr ;

           return (fun () ->  Some v_2 ))

    <|> case "opt_default_1" (perform

           (* empty *)

           return (fun () ->  None ))

      )

  method label_let_pattern = rule "label_let_pattern" (fun () -> dummy
    <|> case "label_let_pattern_0" (perform

           v_1 <-- self#label_var ;
           token COLON;
           v_3 <-- self#core_type ;

           return (fun () ->  let (lab, pat) = v_1 in (lab, mkpat(Ppat_constraint(pat, v_3))) ))

    <!> case "label_let_pattern_1" (perform

           v_1 <-- self#label_var ;

           return (fun () ->  v_1 ))

      )

  method label_var = rule "label_var" (fun () -> dummy
    <|> case "label_var_0" (perform

           v_1 <-- get_LIDENT;

           return (fun () ->  (v_1, mkpat(Ppat_var v_1)) ))

      )

  method let_pattern = rule "let_pattern" (fun () -> dummy
    <|> case "let_pattern_0" (perform

           v_1 <-- self#pattern ;
           token COLON;
           v_3 <-- self#core_type ;

           return (fun () ->  mkpat(Ppat_constraint(v_1, v_3)) ))

    <!> case "let_pattern_1" (perform

           v_1 <-- self#pattern ;

           return (fun () ->  v_1 ))

      )

(*
  method expr = rule "expr" (fun () -> dummy

    <!> case "expr_1" (perform

           expr <-- self#exprX;
           token COMMA;
           rev_exprs <-- self#expr_comma_list ;

           return (fun () -> build_tuple (expr :: List.rev rev_exprs) (fun exprs -> mkexp(Pexp_tuple exprs))))

    <!> case "expr_2" (perform

           res <-- self#exprX;

           return (fun () -> build res) (* CR jfuruse: we should add this _pos to rhs and !symbol_rloc_ref *)

        )
    )
*)
  method expr = rule "expr" (fun () -> dummy

    <|> case "expr_1" (perform

           expr <-- self#exprX;
           rev_exprs <-- option (perform
                                   token COMMA;
                                   self#expr_comma_list);

           return (fun () -> 
             match rev_exprs with
             | Some rev_exprs -> 
                 build_tuple (expr :: List.rev rev_exprs) (fun exprs -> mkexp(Pexp_tuple exprs))
             | None -> build expr)
        )
    )

  method exprX = leftrec "exprX" self#expr_nonleftrec self#expr_leftrec

  method expr_nonleftrec = (dummy
    <|> case "expr_nonleftrec_0" (perform

           token FOR;
           v_2 <-- self#val_ident ;
           token EQUAL;
           v_4 <-- self#seq_expr ;
           v_5 <-- self#direction_flag ;
           v_6 <-- self#seq_expr ;
           token DO;
           v_8 <-- self#seq_expr ;
           token DONE;

           return (fun () ->  mkexpX(Pexp_for(v_2, v_4, v_6, v_5, v_8)) ))

    <|> case "expr_nonleftrec_1" (perform

           token LPAREN;
           token COLONCOLON;
           token RPAREN;
           token LPAREN;
           v_5 <-- self#expr ;
           token COMMA;
           v_7 <-- self#expr ;
           token RPAREN;

           return (fun () ->  mkexpX(Pexp_construct(Lident "::",
                             Some(ghexp(Pexp_tuple[v_5; v_7])),
                             false)) ))

    (* expr_nonleftrec_2 .. _4 are moved to simple_expr_leftrec *)

    <|> case "expr_nonleftrec_5" (perform

           token FUN;

           (perform                                  
              token LPAREN;
              token TYPE;
              v_4 <-- get_LIDENT;
              token RPAREN;
              v_6 <-- self#fun_def ;

              return (fun () ->  mkexpX(Pexp_newtype(v_4, v_6)) ))

           <!> (perform
                  v_2 <-- self#labeled_simple_pattern ;
                  v_3 <-- self#fun_def ;

                  return (fun () ->  let (l,o,p) = v_2 in mkexpX(Pexp_function(l, o, [p, v_3])) )))

    <|> case "expr_nonleftrec_6" (perform

           token IF;
           v_2 <-- self#seq_expr ;
           token THEN;
           v_4 <-- self#expr ;
           v_6 <-- option (perform
                             token ELSE;
                             self#expr); (* Take as long as possible *)

           return (fun () -> 
             match v_6 with
             | Some v_6 -> mkexpX(Pexp_ifthenelse(v_2, v_4, Some v_6))
             | None -> mkexpX(Pexp_ifthenelse(v_2, v_4, None)) ))

    <|> case "expr_nonleftrec_7" (perform

           token LET;

           (perform                                  
              token MODULE;
              v_3 <-- get_UIDENT;
              v_4 <-- self#module_binding ;
              token IN;
              v_6 <-- self#seq_expr ;

              return (fun () ->  mkexpX(Pexp_letmodule(v_3, v_4, v_6)) ))

           <|> (perform 
                  token OPEN;
                  v_3 <-- self#mod_longident ;
                  token IN;
                  v_5 <-- self#seq_expr ;

                  return (fun () ->  mkexpX(Pexp_open(v_3, v_5)) ))

           <|> (perform

                  v_2 <-- self#rec_flag ;
                  v_3 <-- self#let_bindings ;
                  token IN;
                  v_5 <-- self#seq_expr ;

                  return (fun () ->  mkexpX(Pexp_let(v_2, List.rev v_3, v_5)) ))
    )

    <|> case "expr_nonleftrec_10" (perform

           token MATCH;
           v_2 <-- self#seq_expr ;
           token WITH;
           _v_4 <-- self#opt_bar ;
           v_5 <-- self#match_cases ;

           return (fun () ->  mkexpX(Pexp_match(v_2, List.rev v_5)) ))

    <|> case "expr_nonleftrec_11" (perform

           token TRY;
           v_2 <-- self#seq_expr ;
           token WITH;
           (perform
              _v_4 <-- self#opt_bar ;
              v_5 <-- self#match_cases ;

              return (fun () ->  mkexpX(Pexp_try(v_2, List.rev v_5)) ))

           <!> (perform
                  take_ (* error *);

                return (fun () ->  syntax_error() )))

    <|> case "expr_nonleftrec_12" (perform

           token WHILE;
           v_2 <-- self#seq_expr ;
           token DO;
           v_4 <-- self#seq_expr ;
           token DONE;

           return (fun () ->  mkexpX(Pexp_while(v_2, v_4)) ))

    (* expr_nonleftrec_13 is moved to simple_expr_leftrec *)

    <|> case "expr_nonleftrec_17" (perform

           token FUNCTION;
           _v_2 <-- self#opt_bar ;
           v_3 <-- self#match_cases ;

           return (fun () ->  mkexpX(Pexp_function("", None, List.rev v_3)) ))

    <|> case "expr_nonleftrec_18" (perform

           token OBJECT;
           v_2 <-- self#class_structure ;
           (perform
              token END;

              return (fun () ->  mkexpX (Pexp_object(v_2)) ))
           <|> (perform
                  take_ (* error *);

                  return (fun () ->  unclosed "object" 1 "end" 3 )))

    <|> case "expr_nonleftrec_21" (perform

           token ASSERT;
           v_2 <-- self#simple_expr ;
           (* %prec below_SHARP *)

           return (fun () ->  mkassertX v_2 ))

    <|> case "expr_nonleftrec_22" (perform

           token LAZY;
           v_2 <-- self#simple_expr ;
           (* %prec below_SHARP *)

           return (fun () ->  mkexpX (Pexp_lazy (v_2)) ))

    <|> case "expr_nonleftrec_23" (perform

           v_1 <-- self#additive ;
           v_2 <-- self#exprX ;
           (* %prec prec_unary_plus *)

           return (fun () ->  mkuplus v_1 v_2 ))

    <|> case "expr_nonleftrec_27" (perform

           v_1 <-- self#subtractive ;
           v_2 <-- self#exprX ;
           (* %prec prec_unary_minus *)

           return (fun () ->  mkuminus v_1 v_2 ))

    <!> case "expr_nonleftrec_20" (perform

           v_1 <-- self#label ;
           token LESSMINUS;
           v_3 <-- self#expr ; (* Take as long as possible ? *)

           return (fun () ->  mkexpX(Pexp_setinstvar(v_1, v_3)) ))

    <!> case "expr_nonleftrec_24" (perform

           v_1 <-- self#constr_longident ;
           v_2 <-- self#simple_expr ;
           (* %prec below_SHARP *)

           return (fun () ->  mkexpX(Pexp_construct(v_1, Some v_2, false)) ))

    <!> case "expr_nonleftrec_25" (perform

           v_1 <-- self#name_tag ;
           v_2 <-- self#simple_expr ;
           (* %prec below_SHARP *)

           return (fun () ->  mkexpX(Pexp_variant(v_1, Some v_2)) ))

    <!> case "expr_nonleftrec_26" (perform

           v_1 <-- self#simple_expr ;
           v_2 <-- option self#simple_labeled_expr_list ;

           return (fun () -> 
             match v_2 with
             | Some v_2 -> mkexpX(Pexp_apply(v_1, List.rev v_2)) 
             | None ->            (* %prec below_SHARP *)
                 terminal v_1 ))

      )
  method expr_leftrec v_1 = (dummy
    <|> case "expr_leftrec_0" (perform

           token AMPERAMPER;
           v_3 <-- self#exprX ;

           return (fun () ->  mkinfix v_1 "&&" v_3 ))

    <|> case "expr_leftrec_1" (perform

           token AMPERSAND;
           v_3 <-- self#exprX ;

           return (fun () ->  mkinfix v_1 "&" v_3 ))

    <|> case "expr_leftrec_2" (perform

           token BARBAR;
           v_3 <-- self#exprX ;

           return (fun () ->  mkinfix v_1 "||" v_3 ))

    <|> case "expr_leftrec_3" (perform

           token COLONCOLON;
           v_3 <-- self#exprX ;

           return (fun () ->  infix v_1 "::" v_3 (fun v_1 _ v_3 ->
                             mkexp(Pexp_construct(Lident "::",
                                                  Some(ghexp(Pexp_tuple[v_1;v_3])),
                                                  false)) )))

    <|> case "expr_leftrec_4" (perform

           token COLONEQUAL;
           v_3 <-- self#exprX ;

           return (fun () ->  mkinfix v_1 ":=" v_3 ))

    <|> case "expr_leftrec_5" (perform

           token EQUAL;
           v_3 <-- self#exprX ;

           return (fun () ->  mkinfix v_1 "=" v_3 ))

    <|> case "expr_leftrec_6" (perform

           token GREATER;
           v_3 <-- self#exprX ;

           return (fun () ->  mkinfix v_1 ">" v_3 ))

    <|> case "expr_leftrec_7" (perform

           v_2 <-- get_INFIXOP0;
           v_3 <-- self#exprX ;

           return (fun () ->  mkinfix v_1 v_2 v_3 ))

    <|> case "expr_leftrec_8" (perform

           v_2 <-- get_INFIXOP1;
           v_3 <-- self#exprX ;

           return (fun () ->  mkinfix v_1 v_2 v_3 ))

    <|> case "expr_leftrec_9" (perform

           v_2 <-- get_INFIXOP2;
           v_3 <-- self#exprX ;

           return (fun () ->  mkinfix v_1 v_2 v_3 ))

    <|> case "expr_leftrec_10" (perform

           v_2 <-- get_INFIXOP3;
           v_3 <-- self#exprX ;

           return (fun () ->  mkinfix v_1 v_2 v_3 ))

    <|> case "expr_leftrec_11" (perform

           v_2 <-- get_INFIXOP4;
           v_3 <-- self#exprX ;

           return (fun () ->  mkinfix v_1 v_2 v_3 ))

    <|> case "expr_leftrec_12" (perform

           token LESS;
           v_3 <-- self#exprX ;

           return (fun () ->  mkinfix v_1 "<" v_3 ))

    <|> case "expr_leftrec_13" (perform

           token MINUS;
           v_3 <-- self#exprX ;

           return (fun () ->  mkinfix v_1 "-" v_3 ))

    <|> case "expr_leftrec_14" (perform

           token MINUSDOT;
           v_3 <-- self#exprX ;

           return (fun () ->  mkinfix v_1 "-." v_3 ))

    <|> case "expr_leftrec_15" (perform

           token OR;
           v_3 <-- self#exprX ;

           return (fun () ->  mkinfix v_1 "or" v_3 ))

    <|> case "expr_leftrec_16" (perform

           token PLUS;
           v_3 <-- self#exprX ;

           return (fun () ->  mkinfix v_1 "+" v_3 ))

    <|> case "expr_leftrec_17" (perform

           token PLUSDOT;
           v_3 <-- self#exprX ;

           return (fun () ->  mkinfix v_1 "+." v_3 ))

    <|> case "expr_leftrec_18" (perform

           token STAR;
           v_3 <-- self#exprX ;

           return (fun () ->  mkinfix v_1 "*" v_3 ))

      )

  method simple_expr = rule "simple_expr" (fun () -> dummy

    <|> case "simple_expr_0" (perform

           res <-- self#simple_exprX;

           return (fun () -> build res) (* CR jfuruse: we should add this _pos to rhs and !symbol_rloc_ref *)

        )
    )

  method simple_exprX = leftrec "simple_exprX" self#simple_expr_nonleftrec self#simple_expr_leftrec

  method simple_expr_nonleftrec = (dummy
    <|> case "simple_expr_nonleftrec_2" (perform

           v_1 <-- self#mod_longident ;
           token DOT;
           token LPAREN;
           v_4 <-- self#seq_expr ;
           check_close 
             (token RPAREN)
             (fun () ->  mkexpX(Pexp_open(v_1, v_4)))
             (fun () ->  unclosed "(" 3 ")" 5 ))

    <|> case "simple_expr_nonleftrec_4" (perform

           gen_list 
             (token LBRACELESS) "{<"
             self#field_expr_list 
             self#opt_semi 
             (token GREATERRBRACE) ">}"
             (fun l -> mkexpX(Pexp_override l)))

    <|> case "simple_expr_nonleftrec_6" (perform

           gen_list 
             (token LBRACKET) "["
             self#expr_semi_list
             self#opt_semi 
             (token RBRACKET) "]"
             (fun l -> terminal (reloc_exp (mktailexp l))))

    <|> case "simple_expr_nonleftrec_8" (perform

           gen_list 
             (token LBRACKETBAR) "[|"
             self#expr_semi_list
             self#opt_semi 
             (token BARRBRACKET) "|]"
             (fun l -> mkexpX(Pexp_array l)))

    <|> case "simple_expr_nonleftrec_13" (perform

           token LBRACE;
           v_2 <-- self#record_expr ;
           check_close 
             (token RBRACE)
             (fun () ->  let (exten, fields) = v_2 in mkexpX(Pexp_record(fields, exten)))
             (fun () ->  unclosed "{" 1 "}" 3 ))

    <|> case "simple_expr_nonleftrec_11" (
      (perform

         token BEGIN;
         v_2 <-- self#seq_expr ;
         check_close 
           (token END)
           (fun () ->  terminal (reloc_exp v_2))
           (fun () ->  unclosed "begin" 1 "end" 3))

      <!> (perform

             token BEGIN;
             token END;

             return (fun () ->  mkexpX (Pexp_construct (Lident "()", None, false)) )))

    <|> case "simple_expr_nonleftrec_0" (perform

           token LPAREN;
           token MODULE;
           v_3 <-- self#module_expr ;
           token COLON;
           check_close 
             (perform v_5 <-- self#package_type; token RPAREN; return v_5)
             (fun v_5 ->  mkexpX (Pexp_pack (v_3, v_5)))
             (fun () ->  unclosed "(" 1 ")" 5 ))

    <|> case "simple_expr_nonleftrec_17" (perform

           token BANG;
           v_2 <-- self#simple_exprX ;

           return (fun () ->  mkexp_prefix "!" v_2 (fun v_2 -> mkexp(Pexp_apply(mkoperator "!" 1, ["",v_2])) )))

    <!> case "simple_expr_nonleftrec_10" (perform

           token LPAREN;
           v_2 <-- self#seq_expr ;
           v_3 <-- self#type_constraint ;
           token RPAREN;

           return (fun () ->  let (t, t') = v_3 in mkexpX(Pexp_constraint(v_2, t, t')) ))

    <!> case "simple_expr_nonleftrec_15" (perform

           token LPAREN;
           v_2 <-- self#seq_expr ;
           check_close 
             (token RPAREN)
             (fun () ->  terminal (reloc_exp v_2) )
             (fun () ->  unclosed "(" 1 ")" 3 ))

    <!> case "simple_expr_nonleftrec_21" (perform

           token NEW;
           v_2 <-- self#class_longident ;

           return (fun () ->  mkexpX(Pexp_new(v_2)) ))

    <!> case "simple_expr_nonleftrec_22" (perform

           v_1 <-- get_PREFIXOP;
           v_2 <-- self#simple_exprX ;

           return (fun () ->  mkexp_prefix v_1 v_2 (fun v_2 -> mkexp(Pexp_apply(mkoperator v_1 1, ["",v_2])) )))

    <!> case "simple_expr_nonleftrec_23" (perform

           v_1 <-- self#constant ;

           return (fun () ->  mkexpX(Pexp_constant v_1) ))

    (* type t = F.x must not be parsed as type t = F by simple_expr_nonleftrec_19 *)
    <!> case "simple_expr_nonleftrec_26" (perform

           v_1 <-- self#val_longident ;

           return (fun () ->  mkexpX(Pexp_ident v_1) ))

    <!> case "simple_expr_nonleftrec_24" (perform

           v_1 <-- self#constr_longident ;
           (* %prec prec_constant_constructor *)

           return (fun () ->  mkexpX(Pexp_construct(v_1, None, false)) ))

    <!> case "simple_expr_nonleftrec_25" (perform

           v_1 <-- self#name_tag ;
           (* %prec prec_constant_constructor *)

           return (fun () ->  mkexpX(Pexp_variant(v_1, None)) ))

      )

  method simple_expr_leftrec v_1 = (dummy
    (* LESSMINUS cases moved from expr *)
    <!> case "expr_nonleftrec_2" (perform

           token DOT;
           token LBRACE;
           v_4 <-- self#expr ;
           token RBRACE;
           token LESSMINUS;
           v_7 <-- self#expr ;

           return (fun () ->  mkexp_postfix v_1 "." (fun v_1 -> bigarray_set v_1 v_4 v_7 )))

    <!> case "expr_nonleftrec_3" (perform

           token DOT;
           token LBRACKET;
           v_4 <-- self#seq_expr ;
           token RBRACKET;
           token LESSMINUS;
           v_7 <-- self#expr ;

           return (fun () ->  mkexp_postfix v_1 "." (fun v_1 -> mkexp(Pexp_apply(ghexp(Pexp_ident(array_function "String" "set")),
                         ["", v_1; "",v_4; "",v_7])) )))

    <!> case "expr_nonleftrec_4" (perform

           token DOT;
           token LPAREN;
           v_4 <-- self#seq_expr ;
           token RPAREN;
           token LESSMINUS;
           v_7 <-- self#expr ;

           return (fun () ->  mkexp_postfix v_1 "." (fun v_1 -> mkexp(Pexp_apply(ghexp(Pexp_ident(array_function "Array" "set")),
                         ["",v_1; "",v_4; "",v_7])) )))

    <!> case "expr_nonleftrec_13" (perform

           token DOT;
           v_3 <-- self#label_longident ;
           token LESSMINUS;
           v_5 <-- self#expr ;

           return (fun () ->  mkexp_postfix v_1 "." (fun v_1 -> mkexp(Pexp_setfield(v_1, v_3, v_5)) )))
    (*/ LESSMINUS cases moved from expr *)

    <!> case "simple_expr_leftrec_0" (perform

           token DOT;
           token LBRACE;
           v_4 <-- self#expr ;
           check_close 
             (token RBRACE)
             (fun () ->  mkexp_postfix v_1 "." (fun v_1 -> (bigarray_get v_1 v_4 )))
             (fun () ->  unclosed "{" 3 "}" 5 ))

    <!> case "simple_expr_leftrec_2" (perform

           token DOT;
           token LBRACKET;
           v_4 <-- self#seq_expr ;
           check_close
             (token RBRACKET)
             (fun () ->  mkexp_postfix v_1 "." (fun v_1 -> mkexp(Pexp_apply(ghexp(Pexp_ident(array_function "String" "get")),
                                                                            ["",v_1; "",v_4]))))
             (fun () ->  unclosed "[" 3 "]" 5 ))

    <!> case "simple_expr_leftrec_4" (perform

           token DOT;
           token LPAREN;
           v_4 <-- self#seq_expr ;
           check_close 
             (token RPAREN)
             (fun () ->  mkexp_postfix v_1 "." (fun v_1 -> mkexp(Pexp_apply(ghexp(Pexp_ident(array_function "Array" "get")),
                                                                            ["",v_1; "",v_4]))))
             (fun () ->  unclosed "(" 3 ")" 5 ))

    <!> case "simple_expr_leftrec_6" (perform

           token DOT;
           v_3 <-- self#label_longident ;

           return (fun () ->  mkexp_postfix v_1 "." (fun v_1 -> mkexp(Pexp_field(v_1, v_3)) )))

    <!> case "simple_expr_leftrec_7" (perform

           token SHARP;
           v_3 <-- self#label ;

           return (fun () ->  mkexp_postfix v_1 "#" (fun v_1 -> mkexp(Pexp_send(v_1, v_3)) )))

      )

  method simple_labeled_expr_list = leftrec "simple_labeled_expr_list" self#simple_labeled_expr_list_nonleftrec self#simple_labeled_expr_list_leftrec

  method simple_labeled_expr_list_nonleftrec = (dummy
    <|> case "simple_labeled_expr_list_nonleftrec_0" (perform

           v_1 <-- self#labeled_simple_expr ;

           return (fun () ->  [v_1] ))

      )

  method simple_labeled_expr_list_leftrec v_1 = (dummy
    <|> case "simple_labeled_expr_list_leftrec_0" (perform

           v_2 <-- self#labeled_simple_expr ;

           return (fun () ->  v_2 :: v_1 ))

      )

  method labeled_simple_expr = rule "labeled_simple_expr" (fun () -> dummy
    <|> case "labeled_simple_expr_0" (perform

           v_1 <-- self#label_expr ;

           return (fun () ->  v_1 ))

    <!> case "labeled_simple_expr_1" (perform

           v_1 <-- self#simple_expr ;
           (* %prec below_SHARP *)

           return (fun () ->  ("", v_1) ))

      )

  method label_expr = rule "label_expr" (fun () -> dummy
    <|> case "label_expr_0" (perform

           v_1 <-- get_LABEL;
           v_2 <-- self#simple_expr ;
           (* %prec below_SHARP *)

           return (fun () ->  (v_1, v_2) ))

    <|> case "label_expr_1" (perform

           v_1 <-- get_OPTLABEL;
           v_2 <-- self#simple_expr ;
           (* %prec below_SHARP *)

           return (fun () ->  ("?" ^ v_1, v_2) ))

    <|> case "label_expr_2" (perform

           token QUESTION;
           v_2 <-- self#label_ident ;

           return (fun () ->  ("?" ^ fst v_2, snd v_2) ))

    <|> case "label_expr_3" (perform

           token TILDE;
           v_2 <-- self#label_ident ;

           return (fun () ->  v_2 ))

      )

  method label_ident = rule "label_ident" (fun () -> dummy
    <|> case "label_ident_0" (perform

           v_1 <-- get_LIDENT;

           return (fun () ->  (v_1, mkexp(Pexp_ident(Lident v_1))) ))

      )

  method let_bindings = leftrec "let_bindings" self#let_bindings_nonleftrec self#let_bindings_leftrec

  method let_bindings_nonleftrec = (dummy
    <|> case "let_bindings_nonleftrec_0" (perform

           v_1 <-- self#let_binding ;

           return (fun () ->  [v_1] ))

      )

  method let_bindings_leftrec v_1 = (dummy
    <|> case "let_bindings_leftrec_0" (perform

           token AND;
           v_3 <-- self#let_binding ;

           return (fun () ->  v_3 :: v_1 ))

      )

  method let_binding = rule "let_binding" (fun () -> dummy
    <|> case "let_binding_0" (perform

           v_1 <-- self#val_ident ;
           token COLON;
           v_3 <-- self#typevar_list ;
           token DOT;
           v_5 <-- self#core_type ;
           token EQUAL;
           v_7 <-- self#seq_expr ;

           return (fun () ->  (ghpat(Ppat_constraint({ppat_desc = Ppat_var v_1; ppat_loc = rhs_loc 1},
                               ghtyp(Ptyp_poly(v_3,v_5)))),
         v_7) ))

    <!> case "let_binding_1" (perform

           v_1 <-- self#pattern ;
           token EQUAL;
           v_3 <-- self#seq_expr ;

           return (fun () ->  (v_1, v_3) ))

    <!> case "let_binding_2" (perform

           v_1 <-- self#val_ident ;
           v_2 <-- self#fun_binding ;

           return (fun () ->  ({ppat_desc = Ppat_var v_1; ppat_loc = rhs_loc 1}, v_2) ))

      )

  method fun_binding = rule "fun_binding" (fun () -> dummy
    <|> case "fun_binding_0" (perform

           v_1 <-- self#type_constraint ;
           token EQUAL;
           v_3 <-- self#seq_expr ;

           return (fun () ->  let (t, t') = v_1 in ghexp(Pexp_constraint(v_3, t, t')) ))

    <!> case "fun_binding_1" (perform

           v_1 <-- self#strict_binding ;

           return (fun () ->  v_1 ))

      )

  method strict_binding = rule "strict_binding" (fun () -> dummy
    <|> case "strict_binding_1" (perform

           token EQUAL;
           v_2 <-- self#seq_expr ;

           return (fun () ->  v_2 ))

    <|> case "strict_binding_0" (perform

           token LPAREN;
           token TYPE;
           v_3 <-- get_LIDENT;
           token RPAREN;
           v_5 <-- self#fun_binding ;

           return (fun () ->  mkexp(Pexp_newtype(v_3, v_5)) ))

    <!> case "strict_binding_2" (perform

           v_1 <-- self#labeled_simple_pattern ;
           v_2 <-- self#fun_binding ;

           return (fun () ->  let (l, o, p) = v_1 in ghexp(Pexp_function(l, o, [p, v_2])) ))

      )

  method match_cases = leftrec "match_cases" self#match_cases_nonleftrec self#match_cases_leftrec

  method match_cases_nonleftrec = (dummy
    <|> case "match_cases_nonleftrec_0" (perform

           v_1 <-- self#pattern ;
           v_2 <-- self#match_action ;

           return (fun () ->  [v_1, v_2] ))

      )

  method match_cases_leftrec v_1 = (dummy
    <|> case "match_cases_leftrec_0" (perform

           token BAR;
           v_3 <-- self#pattern ;
           v_4 <-- self#match_action ;

           return (fun () ->  (v_3, v_4) :: v_1 ))

      )

  method fun_def = rule "fun_def" (fun () -> dummy
    <|> case "fun_def_2" (perform

           v_1 <-- self#match_action ;

           return (fun () ->  v_1 ))

    <|> case "fun_def_0" (perform

           token LPAREN;
           token TYPE;
           v_3 <-- get_LIDENT;
           token RPAREN;
           v_5 <-- self#fun_def ;

           return (fun () ->  mkexp(Pexp_newtype(v_3, v_5)) ))

    <!> case "fun_def_1" (perform

           v_1 <-- self#labeled_simple_pattern ;
           v_2 <-- self#fun_def ;

           return (fun () ->  let (l,o,p) = v_1 in ghexp(Pexp_function(l, o, [p, v_2])) ))

      )

  method match_action = rule "match_action" (fun () -> dummy
    <|> case "match_action_0" (perform

           token WHEN;
           v_2 <-- self#seq_expr ;
           token MINUSGREATER;
           v_4 <-- self#seq_expr ;

           return (fun () ->  mkexp(Pexp_when(v_2, v_4)) ))

    <|> case "match_action_1" (perform

           token MINUSGREATER;
           v_2 <-- self#seq_expr ;

           return (fun () ->  v_2 ))

      )

  method expr_comma_list = rule "expr_comma_list" (fun () -> case "expr_comma_list" (perform
      
      vs <-- list_with_sep ~sep:(token COMMA) self#exprX;

      return (fun () -> List.rev vs)

    ))

  method record_expr = rule "record_expr" (fun () -> dummy
    <|> case "record_expr_0" (perform

           v_1 <-- self#simple_expr ;
           token WITH;
           v_3 <-- self#lbl_expr_list ;
           _v_4 <-- self#opt_semi ;

           return (fun () ->  (Some v_1, List.rev v_3) ))

    <!> case "record_expr_1" (perform

           v_1 <-- self#lbl_expr_list ;
           _v_2 <-- self#opt_semi ;

           return (fun () ->  (None, List.rev v_1) ))

      )

  method lbl_expr_list = leftrec "lbl_expr_list" self#lbl_expr_list_nonleftrec self#lbl_expr_list_leftrec

  method lbl_expr_list_nonleftrec = (dummy
    <|> case "lbl_expr_list_nonleftrec_0" (perform

           v_1 <-- self#label_longident ;
           v_3 <-- option (perform
                             token EQUAL;
                             self#expr);

           return (fun () ->  
             match v_3 with
             | Some v_3 -> [v_1,v_3]
             | None -> [v_1, exp_of_label v_1]
           )))

  method lbl_expr_list_leftrec v_1 = (dummy
    <|> case "lbl_expr_list_leftrec_0" (perform

           token SEMI;
           v_3 <-- self#label_longident ;
           v_5 <-- option (perform
                             token EQUAL;
                             self#expr);

           return (fun () ->  
             match v_5 with
             | Some v_5 -> (v_3, v_5) :: v_1
             | None -> (v_3, exp_of_label v_3) :: v_1
           ))

      )

  method field_expr_list = leftrec "field_expr_list" self#field_expr_list_nonleftrec self#field_expr_list_leftrec

  method field_expr_list_nonleftrec = (dummy
    <|> case "field_expr_list_nonleftrec_0" (perform

           v_1 <-- self#label ;
           token EQUAL;
           v_3 <-- self#expr ;

           return (fun () ->  [v_1,v_3] ))

      )

  method field_expr_list_leftrec v_1 = (dummy
    <|> case "field_expr_list_leftrec_0" (perform

           token SEMI;
           v_3 <-- self#label ;
           token EQUAL;
           v_5 <-- self#expr ;

           return (fun () ->  (v_3, v_5) :: v_1 ))

      )

  method expr_semi_list = leftrec "expr_semi_list" self#expr_semi_list_nonleftrec self#expr_semi_list_leftrec

  method expr_semi_list_nonleftrec = (dummy
    <|> case "expr_semi_list_nonleftrec_0" (perform

           v_1 <-- self#expr ;

           return (fun () ->  [v_1] ))

      )

  method expr_semi_list_leftrec v_1 = (dummy
    <|> case "expr_semi_list_leftrec_0" (perform

           token SEMI;
           v_3 <-- self#expr ;

           return (fun () ->  v_3 :: v_1 ))

      )

  method type_constraint = rule "type_constraint" (fun () -> dummy
    <|> case "type_constraint_0" (perform

           token COLON;
           v_2 <-- self#core_type ;
           v_4 <-- option (perform
                             token COLONGREATER;
                             self#core_type) ;

           return (fun () ->  
             match v_4 with
             | Some v_4 -> (Some v_2, Some v_4)
             | None -> (Some v_2, None)))

    <!> case "type_constraint_2" (perform

           token COLON;
           take_ (* error *);

           return (fun () ->  syntax_error() ))

    <!> case "type_constraint_3" (perform

           token COLONGREATER;
           v_2 <-- self#core_type ;

           return (fun () ->  (None, Some v_2) ))

    <!> case "type_constraint_4" (perform

           token COLONGREATER;
           take_ (* error *);

           return (fun () ->  syntax_error() ))

      )

(*
  method pattern = rule "pattern" (fun () -> dummy

    <!> case "pattern_1" (perform

           pattern <-- self#patternX;
           token COMMA;
           rev_patterns <-- self#pattern_comma_list ;

           return (fun () -> build_tuple (pattern :: List.rev rev_patterns) (fun patterns -> mkpat(Ppat_tuple patterns))))

    <!> case "pattern_2" (perform

           res <-- self#patternX;

           return (fun () -> build res) (* CR jfuruse: we should add this _pos to rhs and !pos *)

        )
    )
*)

  method pattern = rule "pattern" (fun () -> dummy

    <|> case "pattern_1" (perform

           pattern <-- self#patternX;
           rev_patterns <-- option (perform
                                      token COMMA;
                                      self#pattern_comma_list);

           return (fun () -> 
             match rev_patterns with
             | Some rev_patterns -> 
                 build_tuple (pattern :: List.rev rev_patterns) (fun patterns -> mkpat(Ppat_tuple patterns))
             | None -> build pattern) (* CR jfuruse: we should add this _pos to rhs and !pos *)

    ))

  method patternX = leftrec "patternX" self#pattern_nonleftrec self#pattern_leftrec

  method pattern_nonleftrec = (dummy
    <|> case "pattern_nonleftrec_1" (perform

           token LAZY;
           v_2 <-- self#simple_pattern ;

           return (fun () ->  mkpatX(Ppat_lazy v_2) ))

    <|> case "pattern_nonleftrec_2" (perform

           v_1 <-- self#constr_longident ;
           v_2 <-- self#patternX ;
           (* %prec prec_constr_appl *)

           return (fun () ->  mkpat_prefix 
                                (fun v_2 -> mkpat(Ppat_construct(v_1, Some v_2, false)))
                                "prec_constr_appl" v_2))

    <!> case "pattern_nonleftrec_3" (perform

           v_1 <-- self#name_tag ;
           v_2 <-- self#patternX ;
           (* %prec prec_constr_appl *)

           return (fun () ->  mkpat_prefix
                                (fun v_2 -> mkpat(Ppat_variant(v_1, Some v_2)))
                                "prec_constr_appl" v_2))

    <!> case "pattern_nonleftrec_0" (perform

           token LPAREN;
           token COLONCOLON;
           token RPAREN;
           token LPAREN;
           v_5 <-- self#patternX ;
           token COMMA;
           v_7 <-- self#patternX ;
           token RPAREN;

           return (fun () ->  let v_5, _ = Op_prec.build v_5 in
                              let v_7, _ = Op_prec.build v_7 in
                              mkpatX(Ppat_construct(Lident "::", Some(ghpat(Ppat_tuple[v_5;v_7])),
                             false)) ))

    <!> case "pattern_nonleftrec_4" (perform

           v_1 <-- self#simple_pattern ;

           return (fun () ->  terminal v_1 ))

      )

  method pattern_leftrec v_1 = (dummy
    <|> case "pattern_leftrec_0" (perform

           token AS;
           v_3 <-- self#val_ident ;

           return (fun () ->  mkpat_postfix 
                                (fun v_1 -> mkpat(Ppat_alias(v_1, v_3)))
                                "as" v_1))

    <|> case "pattern_leftrec_1" (perform

           token BAR;
           v_3 <-- self#patternX ;

           return (fun () ->  mkpat_infix v_1 "|" v_3 (fun v_1 v_3 -> mkpat(Ppat_or(v_1, v_3))) ))

    <|> case "pattern_leftrec_2" (perform

           token COLONCOLON;
           v_3 <-- self#patternX ;

           return (fun () ->  mkpat_infix v_1 "::" v_3 (fun v_1 v_3 -> mkpat(Ppat_construct(Lident "::", Some(ghpat(Ppat_tuple[v_1; v_3])),
                             false))) ))

    )
 
  method simple_pattern = rule "simple_pattern" (fun () -> dummy
    <|> case "simple_pattern_0" (perform

           token LPAREN;
           v_2 <-- self#pattern ;
           token COLON;
           v_4 <-- self#core_type ;
           check_close
             (token RPAREN)
             (fun () ->  mkpat(Ppat_constraint(v_2, v_4)))
             (fun () ->  unclosed "(" 1 ")" 5 ))

    <|> case "simple_pattern_3" (perform

           token LBRACE;
           v_2 <-- self#lbl_pattern_list ;
           v_3 <-- self#record_pattern_end ;
           check_close 
             (token RBRACE)
             (fun () ->  mkpat(Ppat_record(List.rev v_2, v_3)))
             (fun () ->  unclosed "{" 1 "}" 4 ))

    <|> case "simple_pattern_4" (perform

           token LBRACKET;
           v_2 <-- self#pattern_semi_list ;
           _v_3 <-- self#opt_semi ;
           check_close 
             (token RBRACKET)
             (fun () ->  reloc_pat (mktailpat (List.rev v_2))) 
             (fun () ->  unclosed "[" 1 "]" 4)) 

    <|> case "simple_pattern_6" (perform

           gen_list
             (token LBRACKETBAR) "[|"
             self#pattern_semi_list 
             self#opt_semi
             (token BARRBRACKET) "|]"
             (fun l ->  mkpat(Ppat_array l)))

    <|> case "simple_pattern_8" (perform

           v_1 <-- get_CHAR;
           token DOTDOT;
           v_3 <-- get_CHAR;

           return (fun () ->  mkrangepat v_1 v_3 ))

    <!> case "simple_pattern_9" (perform

           token LPAREN;
           v_2 <-- self#pattern ;
           check_close
             (token RPAREN)
             (fun () ->  reloc_pat v_2 )
             (fun () ->  unclosed "(" 1 ")" 3 ))

    <!> case "simple_pattern_12" (perform

           token SHARP;
           v_2 <-- self#type_longident ;

           return (fun () ->  mkpat(Ppat_type v_2) ))

    <!> case "simple_pattern_13" (perform

           token UNDERSCORE;

           return (fun () ->  mkpat(Ppat_any) ))

    <!> case "simple_pattern_14" (perform

           v_1 <-- self#constr_longident ;

           return (fun () ->  mkpat(Ppat_construct(v_1, None, false)) ))

    <!> case "simple_pattern_15" (perform

           v_1 <-- self#name_tag ;

           return (fun () ->  mkpat(Ppat_variant(v_1, None)) ))

    <!> case "simple_pattern_16" (perform

           v_1 <-- self#signed_constant ;

           return (fun () ->  mkpat(Ppat_constant v_1) ))

    <!> case "simple_pattern_17" (perform

           v_1 <-- self#val_ident ;
           (* %prec below_EQUAL *)

           return (fun () ->  mkpat(Ppat_var v_1) ))

      )

  method pattern_comma_list = rule "pattern_comma_list" (fun () -> case "pattern_comma_list" (perform
      
      vs <-- list_with_sep ~sep:(token COMMA) self#patternX;

      return (fun () -> List.rev vs)

    ))

  method pattern_semi_list = leftrec "pattern_semi_list" self#pattern_semi_list_nonleftrec self#pattern_semi_list_leftrec

  method pattern_semi_list_nonleftrec = (dummy
    <|> case "pattern_semi_list_nonleftrec_0" (perform

           v_1 <-- self#pattern ;

           return (fun () ->  [v_1] ))

      )

  method pattern_semi_list_leftrec v_1 = (dummy
    <|> case "pattern_semi_list_leftrec_0" (perform

           token SEMI;
           v_3 <-- self#pattern ;

           return (fun () ->  v_3 :: v_1 ))

      )

  method lbl_pattern_list = leftrec "lbl_pattern_list" self#lbl_pattern_list_nonleftrec self#lbl_pattern_list_leftrec

  method lbl_pattern_list_nonleftrec = (dummy
    <|> case "lbl_pattern_list_nonleftrec_0" (perform

           v_1 <-- self#label_longident ;
           token EQUAL;
           v_3 <-- self#pattern ;

           return (fun () ->  [(v_1, v_3)] ))

    <!> case "lbl_pattern_list_nonleftrec_1" (perform

           v_1 <-- self#label_longident ;

           return (fun () ->  [(v_1, pat_of_label v_1)] ))

      )

  method lbl_pattern_list_leftrec v_1 = (dummy
    <|> case "lbl_pattern_list_leftrec_0" (perform

           token SEMI;
           v_3 <-- self#label_longident ;
           token EQUAL;
           v_5 <-- self#pattern ;

           return (fun () ->  (v_3, v_5) :: v_1 ))

    <!> case "lbl_pattern_list_leftrec_1" (perform

           token SEMI;
           v_3 <-- self#label_longident ;

           return (fun () ->  (v_3, pat_of_label v_3) :: v_1 ))

      )

  method record_pattern_end = rule "record_pattern_end" (fun () -> dummy
    <|> case "record_pattern_end_0" (perform

           token SEMI;
           token UNDERSCORE;
           _v_3 <-- self#opt_semi ;

           return (fun () ->  Open ))

    <!> case "record_pattern_end_1" (perform

           _v_1 <-- self#opt_semi ;

           return (fun () ->  Closed ))

      )

  method primitive_declaration = rule "primitive_declaration" (fun () -> dummy
    <|> case "primitive_declaration_0" (perform

           v_1 <-- get_STRING;
           v_2 <-- option (self#primitive_declaration);

           return (fun () ->  
             match v_2 with
             | Some v_2 -> v_1 :: v_2 
             | None -> [v_1]))

      )

  method type_declarations = leftrec "type_declarations" self#type_declarations_nonleftrec self#type_declarations_leftrec

  method type_declarations_nonleftrec = (dummy
    <|> case "type_declarations_nonleftrec_0" (perform

           v_1 <-- self#type_declaration ;

           return (fun () ->  [v_1] ))

      )

  method type_declarations_leftrec v_1 = (dummy
    <|> case "type_declarations_leftrec_0" (perform

           token AND;
           v_3 <-- self#type_declaration ;

           return (fun () ->  v_3 :: v_1 ))

      )

  method type_declaration = rule "type_declaration" (fun () -> dummy
    <|> case "type_declaration_0" (perform

           v_1 <-- self#type_parameters ;
           v_2 <-- get_LIDENT;
           v_3 <-- self#type_kind ;
           v_4 <-- self#constraints ;

           return (fun () ->  let (params, variance) = List.split v_1 in
        let (kind, private_flag, manifest) = v_3 in
        (v_2, {ptype_params = params;
              ptype_cstrs = List.rev v_4;
              ptype_kind = kind;
              ptype_private = private_flag;
              ptype_manifest = manifest;
              ptype_variance = variance;
              ptype_loc = symbol_rloc()}) ))

      )

  method constraints = leftrec "constraints" self#constraints_nonleftrec self#constraints_leftrec

  method constraints_nonleftrec = (dummy
    <|> case "constraints_nonleftrec_0" (perform

           (* empty *)

           return (fun () ->  [] ))

      )

  method constraints_leftrec v_1 = (dummy
    <|> case "constraints_leftrec_0" (perform

           token CONSTRAINT;
           v_3 <-- self#constrain ;

           return (fun () ->  v_3 :: v_1 ))

      )

  method type_kind = rule "type_kind" (fun () -> dummy
    <|> case "type_kind_0" (perform

           token EQUAL;
           v_2 <-- self#core_type ;
           token EQUAL;
           v_4 <-- self#private_flag ;
           token LBRACE;
           v_6 <-- self#label_declarations ;
           _v_7 <-- self#opt_semi ;
           token RBRACE;

           return (fun () ->  (Ptype_record(List.rev v_6), v_4, Some v_2) ))

    <!> case "type_kind_1" (perform

           token EQUAL;
           v_2 <-- self#core_type ;
           token EQUAL;
           v_4 <-- self#private_flag ;
           _v_5 <-- self#opt_bar ;
           v_6 <-- self#constructor_declarations ;

           return (fun () ->  (Ptype_variant(List.rev v_6), v_4, Some v_2) ))

    <!> case "type_kind_2" (perform

           token EQUAL;
           v_2 <-- self#private_flag ;
           token LBRACE;
           v_4 <-- self#label_declarations ;
           _v_5 <-- self#opt_semi ;
           token RBRACE;

           return (fun () ->  (Ptype_record(List.rev v_4), v_2, None) ))

    <!> case "type_kind_3" (perform

           token EQUAL;
           v_2 <-- self#private_flag ;
           token BAR;
           v_4 <-- self#constructor_declarations ;

           return (fun () ->  (Ptype_variant(List.rev v_4), v_2, None) ))

    (* type t = private F.x must not be parsed as type t = F by "type_kind_4" *)
    <!> case "type_kind_5" (perform

           token EQUAL;
           v_2 <-- option (token PRIVATE);
           v_3 <-- self#core_type ;

           return (fun () -> 
             match v_2 with
             | Some () -> (Ptype_abstract, Private, Some v_3)
             | None -> (Ptype_abstract, Public, Some v_3)))

    <!> case "type_kind_4" (perform

           token EQUAL;
           v_2 <-- option (token PRIVATE);
           v_3 <-- self#constructor_declarations ;

           return (fun () -> 
             match v_2 with
             | Some () -> (Ptype_variant(List.rev v_3), Private, None)
             | None -> (Ptype_variant(List.rev v_3), Public, None)))

    <|> case "type_kind_8" (perform

           (* empty *)

           return (fun () ->  (Ptype_abstract, Public, None) ))

      )

  method type_parameters = rule "type_parameters" (fun () -> dummy
    <|> case "type_parameters_0" (perform

           token LPAREN;
           v_2 <-- self#type_parameter_list ;
           token RPAREN;

           return (fun () ->  List.rev v_2 ))

    <|> case "type_parameters_1" (perform

           v_1 <-- self#type_parameter ;

           return (fun () ->  [v_1] ))

    <|> case "type_parameters_2" (perform

           (* empty *)

           return (fun () ->  [] ))

      )

  method type_parameter = rule "type_parameter" (fun () -> dummy
    <|> case "type_parameter_0" (perform

           v_1 <-- self#type_variance ;
           token QUOTE;
           v_3 <-- self#ident ;

           return (fun () ->  v_3, v_1 ))

      )

  method type_variance = rule "type_variance" (fun () -> dummy
    <|> case "type_variance_0" (perform

           token MINUS;

           return (fun () ->  false, true ))

    <|> case "type_variance_1" (perform

           token PLUS;

           return (fun () ->  true, false ))

    <|> case "type_variance_2" (perform

           (* empty *)

           return (fun () ->  false, false ))

      )

  method type_parameter_list = leftrec "type_parameter_list" self#type_parameter_list_nonleftrec self#type_parameter_list_leftrec

  method type_parameter_list_nonleftrec = (dummy
    <|> case "type_parameter_list_nonleftrec_0" (perform

           v_1 <-- self#type_parameter ;

           return (fun () ->  [v_1] ))

      )

  method type_parameter_list_leftrec v_1 = (dummy
    <|> case "type_parameter_list_leftrec_0" (perform

           token COMMA;
           v_3 <-- self#type_parameter ;

           return (fun () ->  v_3 :: v_1 ))

      )

  method constructor_declarations = leftrec "constructor_declarations" self#constructor_declarations_nonleftrec self#constructor_declarations_leftrec

  method constructor_declarations_nonleftrec = (dummy
    <|> case "constructor_declarations_nonleftrec_0" (perform

           v_1 <-- self#constructor_declaration ;

           return (fun () ->  [v_1] ))

      )

  method constructor_declarations_leftrec v_1 = (dummy
    <|> case "constructor_declarations_leftrec_0" (perform

           token BAR;
           v_3 <-- self#constructor_declaration ;

           return (fun () ->  v_3 :: v_1 ))

      )

  method constructor_declaration = rule "constructor_declaration" (fun () -> dummy
    <|> case "constructor_declaration_0" (perform

           v_1 <-- self#constr_ident ;
           v_2 <-- self#constructor_arguments ;

           return (fun () ->  (v_1, v_2, symbol_rloc()) ))

      )

  method constructor_arguments = rule "constructor_arguments" (fun () -> dummy
    <|> case "constructor_arguments_0" (perform

           token OF;
           v_2 <-- self#core_type_list ;

           return (fun () ->  List.rev v_2 ))

    <|> case "constructor_arguments_1" (perform

           (* empty *)

           return (fun () ->  [] ))

      )

  method label_declarations = leftrec "label_declarations" self#label_declarations_nonleftrec self#label_declarations_leftrec

  method label_declarations_nonleftrec = (dummy
    <|> case "label_declarations_nonleftrec_0" (perform

           v_1 <-- self#label_declaration ;

           return (fun () ->  [v_1] ))

      )

  method label_declarations_leftrec v_1 = (dummy
    <|> case "label_declarations_leftrec_0" (perform

           token SEMI;
           v_3 <-- self#label_declaration ;

           return (fun () ->  v_3 :: v_1 ))

      )

  method label_declaration = rule "label_declaration" (fun () -> dummy
    <|> case "label_declaration_0" (perform

           v_1 <-- self#mutable_flag ;
           v_2 <-- self#label ;
           token COLON;
           v_4 <-- self#poly_type ;

           return (fun () ->  (v_2, v_1, v_4, symbol_rloc()) ))

      )

  method with_constraints = leftrec "with_constraints" self#with_constraints_nonleftrec self#with_constraints_leftrec

  method with_constraints_nonleftrec = (dummy
    <|> case "with_constraints_nonleftrec_0" (perform

           v_1 <-- self#with_constraint ;

           return (fun () ->  [v_1] ))

      )

  method with_constraints_leftrec v_1 = (dummy
    <|> case "with_constraints_leftrec_0" (perform

           token AND;
           v_3 <-- self#with_constraint ;

           return (fun () ->  v_3 :: v_1 ))

      )

  method with_constraint = rule "with_constraint" (fun () -> dummy
    <|> case "with_constraint_2" (perform

           token MODULE;
           v_2 <-- self#mod_longident ;
           v_3 <-- one_of [COLONEQUAL; EQUAL];
           v_4 <-- self#mod_ext_longident ;

           return (fun () ->  (v_2, 
                               match v_3 with
                               | COLONEQUAL -> Pwith_modsubst v_4
                               | EQUAL -> Pwith_module v_4
                               | _ -> assert false)))

    <|> case "with_constraint_0" (perform

           token TYPE;
           v_2 <-- self#type_parameters ;
           v_3 <-- self#label_longident ;
           v_4 <-- self#with_type_binder ;
           v_5 <-- self#core_type ;
           v_6 <-- self#constraints ;

           return (fun () ->  let params, variance = List.split v_2 in
        (v_3, Pwith_type {ptype_params = params;
                         ptype_cstrs = List.rev v_6;
                         ptype_kind = Ptype_abstract;
                         ptype_manifest = Some v_5;
                         ptype_private = v_4;
                         ptype_variance = variance;
                         ptype_loc = symbol_rloc()}) ))

    <!> case "with_constraint_1" (perform

           token TYPE;
           v_2 <-- self#type_parameters ;
           v_3 <-- self#label_longident ;
           token COLONEQUAL;
           v_5 <-- self#core_type ;

           return (fun () ->  let params, variance = List.split v_2 in
        (v_3, Pwith_typesubst {ptype_params = params;
                              ptype_cstrs = [];
                              ptype_kind = Ptype_abstract;
                              ptype_manifest = Some v_5;
                              ptype_private = Public;
                              ptype_variance = variance;
                              ptype_loc = symbol_rloc()}) ))

      )

  method with_type_binder = rule "with_type_binder" (fun () -> dummy
    <|> case "with_type_binder_0" (perform

           token EQUAL;
           v_2 <-- option (token PRIVATE);

           return (fun () -> 
             match v_2 with
             | Some () -> Private 
             | None -> Public ))

      )

  method typevar_list = leftrec "typevar_list" self#typevar_list_nonleftrec self#typevar_list_leftrec

  method typevar_list_nonleftrec = (dummy
    <|> case "typevar_list_nonleftrec_0" (perform

           token QUOTE;
           v_2 <-- self#ident ;

           return (fun () ->  [v_2] ))

      )

  method typevar_list_leftrec v_1 = (dummy
    <|> case "typevar_list_leftrec_0" (perform

           token QUOTE;
           v_3 <-- self#ident ;

           return (fun () ->  v_3 :: v_1 ))

      )

  method poly_type = rule "poly_type" (fun () -> dummy
    <|> case "poly_type_0" (perform

           v_1 <-- self#typevar_list ;
           token DOT;
           v_3 <-- self#core_type ;

           return (fun () ->  mktyp(Ptyp_poly(List.rev v_1, v_3)) ))

    <!> case "poly_type_1" (perform

           v_1 <-- self#core_type ;

           return (fun () ->  mktyp(Ptyp_poly([], v_1)) ))
      )

  method core_type = rule "core_type" (fun () -> dummy
    <|> case "core_type_0" (perform

           v_1 <-- self#core_type2 ;
           token AS;
           token QUOTE;
           v_4 <-- self#ident ;

           return (fun () ->  mktyp(Ptyp_alias(v_1, v_4)) ))

    <!> case "core_type_1" (perform

           v_1 <-- self#core_type2 ;

           return (fun () ->  v_1 ))

      )

  method core_type2 = leftrec "core_type2" self#core_type2_nonleftrec self#core_type2_leftrec

  method core_type2_nonleftrec = (dummy
    <|> case "core_type2_nonleftrec_0" (perform

           token QUESTION;
           v_2 <-- get_LIDENT;
           token COLON;
           (* v_4 <-- self#core_type2 ; *)
           v_4 <-- self#simple_core_type_or_tuple ; (* ??? *)
           token MINUSGREATER;
           v_6 <-- self#core_type2 ;

           return (fun () ->  mktyp(Ptyp_arrow("?" ^ v_2 ,
               {ptyp_desc = Ptyp_constr(Ldot (Lident "*predef*", "option"), [v_4]);
                ptyp_loc = v_4.ptyp_loc}, v_6)) ))

    <|> case "core_type2_nonleftrec_2" (perform

           v_1 <-- get_OPTLABEL;
           (* v_2 <-- self#core_type2 ; *) (* eats too much *)
           v_2 <-- self#simple_core_type_or_tuple ; (* ??? *)
           token MINUSGREATER;
           v_4 <-- self#core_type2 ;

           return (fun () ->  mktyp(Ptyp_arrow("?" ^ v_1 ,
               {ptyp_desc = Ptyp_constr(Ldot (Lident "*predef*", "option"), [v_2]);
                ptyp_loc = v_2.ptyp_loc}, v_4)) ))

    <|> case "core_type2_nonleftrec_1" (perform

           v_1 <-- get_LIDENT;
           token COLON;
           (* v_3 <-- self#core_type2 ; *)
           v_3 <-- self#simple_core_type_or_tuple ; (* ??? *)
           token MINUSGREATER;
           v_5 <-- self#core_type2 ;

           return (fun () ->  mktyp(Ptyp_arrow(v_1, v_3, v_5)) ))

    <!> case "core_type2_nonleftrec_3" (perform

           v_1 <-- self#simple_core_type_or_tuple ; (* may start with LIDENT *)

           return (fun () ->  v_1 ))

      )

  method core_type2_leftrec v_1 = (dummy
    <|> case "core_type2_leftrec_0" (perform

           token MINUSGREATER;
           v_3 <-- self#core_type2 ;

           return (fun () ->  mktyp(Ptyp_arrow("", v_1, v_3)) ))

      )

  method simple_core_type = rule "simple_core_type" (fun () -> dummy
    (* to prevent type t = (int * float) s is parsed as type t = (int * float) by simple_core_type_0 *)
    <|> case "simple_core_type_1" (perform

           v_1 <-- self#simple_core_type2 ; (* with LPAREN ... *)
           (* %prec below_SHARP *)

           return (fun () ->  v_1 ))

    <!> case "simple_core_type_0" (perform

           token LPAREN;
           v_2 <-- self#core_type_comma_list ;
           token RPAREN;
           (* %prec below_SHARP *)

           return (fun () ->  match v_2 with [sty] -> sty | _ -> raise Parse_error ))

      )

  method simple_core_type2 = leftrec "simple_core_type2" self#simple_core_type2_nonleftrec self#simple_core_type2_leftrec

  method simple_core_type2_nonleftrec = (dummy
    <|> case "simple_core_type2_nonleftrec_15" (perform

           v_1 <-- self#type_longident ;

           return (fun () ->  mktyp(Ptyp_constr(v_1, [])) ))

    <|> case "simple_core_type2_nonleftrec_10" (perform

           token SHARP;
           v_2 <-- self#class_longident ;
           v_3 <-- self#opt_present ;

           return (fun () ->  mktyp(Ptyp_class(v_2, [], v_3)) ))

    <|> case "simple_core_type2_nonleftrec_13" (perform

           token QUOTE;
           v_2 <-- self#ident ;

           return (fun () ->  mktyp(Ptyp_var v_2) ))

    <|> case "simple_core_type2_nonleftrec_14" (perform

           token UNDERSCORE;

           return (fun () ->  mktyp(Ptyp_any) ))

    <|> case "simple_core_type2_nonleftrec_9" (perform

           token LESS;
           v_2 <-- option self#meth_list ;
           token GREATER;

           return (fun () ->  
             match v_2 with
             | Some v_2 -> mktyp(Ptyp_object v_2)
             | None -> mktyp(Ptyp_object [])))

    <|> case "simple_core_type2_nonleftrec_0" (perform

           token LBRACKETLESS;
           _v_2 <-- self#opt_bar ;
           v_3 <-- self#row_field_list ;
           v_5 <-- option (perform
                             token GREATER;
                             self#name_tag_list); 
           token RBRACKET;

           return (fun () -> 
             match v_5 with
             | Some v_5 -> mktyp(Ptyp_variant(List.rev v_3, true, Some (List.rev v_5)))
             | None -> mktyp(Ptyp_variant(List.rev v_3, true, Some []))))

    <|> case "simple_core_type2_nonleftrec_4" (perform

           token LBRACKETGREATER;
           v_3 <-- option (perform                                               
                             self#opt_bar ;
                             self#row_field_list);
           token RBRACKET;

           return (fun () -> 
             match v_3 with
             | Some v_3 -> mktyp(Ptyp_variant(List.rev v_3, false, None))
             | None -> mktyp(Ptyp_variant([], false, None))))

    <|> case "simple_core_type2_nonleftrec_6" (perform

           token LPAREN;
           token MODULE;
           v_3 <-- self#package_type ;
           token RPAREN;

           return (fun () ->  mktyp(Ptyp_package v_3) ))

    <!> case "simple_core_type2_nonleftrec_1" (perform

           token LPAREN;
           v_2 <-- self#core_type_comma_list ;
           token RPAREN;
           token SHARP;
           v_5 <-- self#class_longident ;
           v_6 <-- self#opt_present ;

           return (fun () ->  mktyp(Ptyp_class(v_5, List.rev v_2, v_6)) ))

    <!> case "simple_core_type2_nonleftrec_7" (perform

           token LPAREN;
           v_2 <-- self#core_type_comma_list ;
           token RPAREN;
           v_4 <-- self#type_longident ;

           return (fun () ->  mktyp(Ptyp_constr(v_4, List.rev v_2)) ))

    <!> case "simple_core_type2_nonleftrec_2" (perform

           token LBRACKET;
           v_2 <-- self#row_field ;
           token BAR;
           v_4 <-- self#row_field_list ;
           token RBRACKET;

           return (fun () ->  mktyp(Ptyp_variant(v_2 :: List.rev v_4, true, None)) ))

    <!> case "simple_core_type2_nonleftrec_3" (perform

           token LBRACKET;
           token BAR;
           v_3 <-- self#row_field_list ;
           token RBRACKET;

           return (fun () ->  mktyp(Ptyp_variant(List.rev v_3, true, None)) ))

    <!> case "simple_core_type2_nonleftrec_8" (perform

           token LBRACKET;
           v_2 <-- self#tag_field ;
           token RBRACKET;

           return (fun () ->  mktyp(Ptyp_variant([v_2], true, None)) ))

      )

  method simple_core_type2_leftrec v_1 = (dummy
    <|> case "simple_core_type2_leftrec_0" (perform

           token SHARP;
           v_3 <-- self#class_longident ;
           v_4 <-- self#opt_present ;

           return (fun () ->  mktyp(Ptyp_class(v_3, [v_1], v_4)) ))

    <|> case "simple_core_type2_leftrec_1" (perform

           v_2 <-- self#type_longident ;

           return (fun () ->  mktyp(Ptyp_constr(v_2, [v_1])) ))

      )

  method package_type = rule "package_type" (fun () -> dummy
    <|> case "package_type_0" (perform

           v_1 <-- self#mty_longident ;
           v_3 <-- option (perform
                             token WITH;
                             self#package_type_cstrs);

           return (fun () ->  
             match v_3 with
             | Some v_3 -> (v_1, v_3) 
             | None -> (v_1, [])))

      )

  method package_type_cstr = rule "package_type_cstr" (fun () -> dummy
    <|> case "package_type_cstr_0" (perform

           token TYPE;
           v_2 <-- get_LIDENT;
           token EQUAL;
           v_4 <-- self#core_type ;

           return (fun () ->  (v_2, v_4) ))

      )

(* TYPICAL INEFFICIENCY 
  method package_type_cstrs = rule "package_type_cstrs" (fun () -> dummy
    <|> case "package_type_cstrs_0" (perform

           v_1 <-- self#package_type_cstr ;
           token AND;
           v_3 <-- self#package_type_cstrs ;

           return (fun () ->  v_1::v_3 ))

    <!> case "package_type_cstrs_1" (perform

           v_1 <-- self#package_type_cstr ;

           return (fun () ->  [v_1] ))

      )
*)

  method package_type_cstrs = rule "package_type_cstrs" (fun () -> dummy
    <|> case "package_type_cstrs_0" (perform

           v_1 <-- self#package_type_cstr ;
           v_3 <-- option (perform
                             token AND;
                             self#package_type_cstrs);

           return (fun () ->  
             match v_3 with
             | Some v_3 -> v_1::v_3
             | None -> [v_1]))

      )

  method row_field_list = leftrec "row_field_list" self#row_field_list_nonleftrec self#row_field_list_leftrec

  method row_field_list_nonleftrec = (dummy
    <|> case "row_field_list_nonleftrec_0" (perform

           v_1 <-- self#row_field ;

           return (fun () ->  [v_1] ))

      )

  method row_field_list_leftrec v_1 = (dummy
    <|> case "row_field_list_leftrec_0" (perform

           token BAR;
           v_3 <-- self#row_field ;

           return (fun () ->  v_3 :: v_1 ))

      )

  method row_field = rule "row_field" (fun () -> dummy
    <|> case "row_field_1" (perform

           v_1 <-- self#tag_field ;

           return (fun () ->  v_1 ))

    <|> case "row_field_0" (perform

           v_1 <-- self#simple_core_type2 ;

           return (fun () ->  Rinherit v_1 ))

      )

  method tag_field = rule "tag_field" (fun () -> dummy
    <|> case "tag_field_0" (perform

           v_1 <-- self#name_tag ;
           v_3_4 <-- option (perform
                               token OF;
                               v_3 <-- self#opt_ampersand ;
                               v_4 <-- self#amper_type_list ;
                               return (v_3, v_4));

           return (fun () ->  
             match v_3_4 with
             | Some (v_3, v_4) ->
                 Rtag (v_1, v_3, List.rev v_4)
             | None -> Rtag (v_1, true, []) ))

      )

  method opt_ampersand = rule "opt_ampersand" (fun () -> dummy
    <|> case "opt_ampersand_0" (perform

           token AMPERSAND;

           return (fun () ->  true ))

    <|> case "opt_ampersand_1" (perform

           (* empty *)

           return (fun () ->  false ))

      )

  method amper_type_list = leftrec "amper_type_list" self#amper_type_list_nonleftrec self#amper_type_list_leftrec

  method amper_type_list_nonleftrec = (dummy
    <|> case "amper_type_list_nonleftrec_0" (perform

           v_1 <-- self#core_type ;

           return (fun () ->  [v_1] ))

      )

  method amper_type_list_leftrec v_1 = (dummy
    <|> case "amper_type_list_leftrec_0" (perform

           token AMPERSAND;
           v_3 <-- self#core_type ;

           return (fun () ->  v_3 :: v_1 ))

      )

  method opt_present = rule "opt_present" (fun () -> dummy
    <|> case "opt_present_0" (perform

           token LBRACKETGREATER;
           v_2 <-- self#name_tag_list ;
           token RBRACKET;

           return (fun () ->  List.rev v_2 ))

    <|> case "opt_present_1" (perform

           (* empty *)

           return (fun () ->  [] ))

      )

  method name_tag_list = leftrec "name_tag_list" self#name_tag_list_nonleftrec self#name_tag_list_leftrec

  method name_tag_list_nonleftrec = (dummy
    <|> case "name_tag_list_nonleftrec_0" (perform

           v_1 <-- self#name_tag ;

           return (fun () ->  [v_1] ))

      )

  method name_tag_list_leftrec v_1 = (dummy
    <|> case "name_tag_list_leftrec_0" (perform

           v_2 <-- self#name_tag ;

           return (fun () ->  v_2 :: v_1 ))

      )

  method simple_core_type_or_tuple = rule "simple_core_type_or_tuple" (fun () -> dummy
    <|> case "simple_core_type_or_tuple_0" (perform

           v_1 <-- self#simple_core_type ;
           v_3 <-- option( perform
                             token STAR;
                             self#core_type_list); 

           return (fun () -> 
             match v_3 with
             | Some v_3 -> mktyp(Ptyp_tuple(v_1 :: List.rev v_3))
             | None -> v_1))

      )

  method core_type_comma_list = leftrec "core_type_comma_list" self#core_type_comma_list_nonleftrec self#core_type_comma_list_leftrec

  method core_type_comma_list_nonleftrec = (dummy
    <|> case "core_type_comma_list_nonleftrec_0" (perform

           v_1 <-- self#core_type ;

           return (fun () ->  [v_1] ))

      )

  method core_type_comma_list_leftrec v_1 = (dummy
    <|> case "core_type_comma_list_leftrec_0" (perform

           token COMMA;
           v_3 <-- self#core_type ;

           return (fun () ->  v_3 :: v_1 ))

      )

  method core_type_list = leftrec "core_type_list" self#core_type_list_nonleftrec self#core_type_list_leftrec

  method core_type_list_nonleftrec = (dummy
    <|> case "core_type_list_nonleftrec_0" (perform

           v_1 <-- self#simple_core_type ;

           return (fun () ->  [v_1] ))

      )

  method core_type_list_leftrec v_1 = (dummy
    <|> case "core_type_list_leftrec_0" (perform

           token STAR;
           v_3 <-- self#simple_core_type ;

           return (fun () ->  v_3 :: v_1 ))

      )

  method meth_list = rule "meth_list" (fun () -> dummy
    <|> case "meth_list_2" (perform

           token DOTDOT;

           return (fun () ->  [mkfield Pfield_var] ))

    <|> case "meth_list_0" (perform

           v_1 <-- self#field ;
           token SEMI;
           v_3 <-- self#meth_list ;

           return (fun () ->  v_1 :: v_3 ))

    <!> case "meth_list_1" (perform

           v_1 <-- self#field ;
           _v_2 <-- self#opt_semi ;

           return (fun () ->  [v_1] ))

      )

  method field = rule "field" (fun () -> dummy
    <|> case "field_0" (perform

           v_1 <-- self#label ;
           token COLON;
           v_3 <-- self#poly_type ;

           return (fun () ->  mkfield(Pfield(v_1, v_3)) ))

      )

  method label = rule "label" (fun () -> dummy
    <|> case "label_0" (perform

           v_1 <-- get_LIDENT;

           return (fun () ->  v_1 ))

      )

  method constant = rule "constant" (fun () -> dummy
    <|> case "constant_0" (perform

           v_1 <-- get_CHAR;

           return (fun () ->  Const_char v_1 ))

    <|> case "constant_1" (perform

           v_1 <-- get_FLOAT;

           return (fun () ->  Const_float v_1 ))

    <|> case "constant_2" (perform

           v_1 <-- get_INT;

           return (fun () ->  Const_int v_1 ))

    <|> case "constant_3" (perform

           v_1 <-- get_INT32;

           return (fun () ->  Const_int32 v_1 ))

    <|> case "constant_4" (perform

           v_1 <-- get_INT64;

           return (fun () ->  Const_int64 v_1 ))

    <|> case "constant_5" (perform

           v_1 <-- get_NATIVEINT;

           return (fun () ->  Const_nativeint v_1 ))

    <|> case "constant_6" (perform

           v_1 <-- get_STRING;

           return (fun () ->  Const_string v_1 ))

      )

  method signed_constant = rule "signed_constant" (fun () -> dummy
    <|> case "signed_constant_0" (perform

           token MINUS;
           t <-- take;
           match t with
           | FLOAT v_2 -> return (fun () -> Const_float("-" ^ v_2))
           | INT v_2 -> return (fun () ->  Const_int(- v_2)) 
           | INT32 v_2 -> return (fun () ->  Const_int32(Int32.neg v_2))
           | INT64 v_2 -> return (fun () ->  Const_int64(Int64.neg v_2))
           | NATIVEINT v_2 -> return (fun () ->  Const_nativeint(Nativeint.neg v_2))
             (* It is only used in pattern, so we do not need to recover the error *)
           | _ -> error "Expected a number literal")

    <|> case "signed_constant_5" (perform

           token PLUS;
           t <-- take;
           match t with
           | FLOAT v_2 -> return (fun () -> Const_float(v_2))
           | INT v_2 -> return (fun () ->  Const_int v_2)
           | INT32 v_2 -> return (fun () ->  Const_int32 v_2)
           | INT64 v_2 -> return (fun () ->  Const_int64 v_2)
           | NATIVEINT v_2 -> return (fun () ->  Const_nativeint v_2)
             (* It is only used in pattern, so we do not need to recover the error *)
           | _ -> error "Expected a number literal")

    <|> case "signed_constant_10" (perform

           v_1 <-- self#constant ;

           return (fun () ->  v_1 ))

      )

  method ident = rule "ident" (fun () -> dummy
    <|> case "ident_0" (perform

           v_1 <-- get_LIDENT;

           return (fun () ->  v_1 ))

    <|> case "ident_1" (perform

           v_1 <-- get_UIDENT;

           return (fun () ->  v_1 ))

      )

  method val_ident = rule "val_ident" (fun () -> dummy
    <|> case "val_ident_0" (perform

           token LPAREN;
           v_2 <-- self#operator ;
           token RPAREN;

           return (fun () ->  v_2 ))

    <|> case "val_ident_1" (perform

           v_1 <-- get_LIDENT;

           return (fun () ->  v_1 ))

      )

  (* used only for the parened form like (&&) *)
  method operator = rule "operator" (fun () -> dummy
    <|> case "operator_0" (perform

           token AMPERAMPER;

           return (fun () ->  "&&" ))

    <|> case "operator_1" (perform

           token AMPERSAND;

           return (fun () ->  "&" ))

    <|> case "operator_2" (perform

           token BANG;

           return (fun () ->  "!" ))

    <|> case "operator_3" (perform

           token BARBAR;

           return (fun () ->  "||" ))

    <|> case "operator_4" (perform

           token COLONEQUAL;

           return (fun () ->  ":=" ))

    <|> case "operator_5" (perform

           token EQUAL;

           return (fun () ->  "=" ))

    <|> case "operator_6" (perform

           token GREATER;

           return (fun () ->  ">" ))

    <|> case "operator_7" (perform

           v_1 <-- get_INFIXOP0;

           return (fun () ->  v_1 ))

    <|> case "operator_8" (perform

           v_1 <-- get_INFIXOP1;

           return (fun () ->  v_1 ))

    <|> case "operator_9" (perform

           v_1 <-- get_INFIXOP2;

           return (fun () ->  v_1 ))

    <|> case "operator_10" (perform

           v_1 <-- get_INFIXOP3;

           return (fun () ->  v_1 ))

    <|> case "operator_11" (perform

           v_1 <-- get_INFIXOP4;

           return (fun () ->  v_1 ))

    <|> case "operator_12" (perform

           token LESS;

           return (fun () ->  "<" ))

    <|> case "operator_13" (perform

           token MINUS;

           return (fun () ->  "-" ))

    <|> case "operator_14" (perform

           token MINUSDOT;

           return (fun () ->  "-." ))

    <|> case "operator_15" (perform

           token OR;

           return (fun () ->  "or" ))

    <|> case "operator_16" (perform

           token PLUS;

           return (fun () ->  "+" ))

    <|> case "operator_17" (perform

           token PLUSDOT;

           return (fun () ->  "+." ))

    <|> case "operator_18" (perform

           v_1 <-- get_PREFIXOP;

           return (fun () ->  v_1 ))

    <|> case "operator_19" (perform

           token STAR;

           return (fun () ->  "*" ))

      )

  method constr_ident = rule "constr_ident" (fun () -> dummy
    <|> case "constr_ident_0" (perform

           token LPAREN;
           token RPAREN;

           return (fun () ->  "()" ))

    <|> case "constr_ident_1" (perform

           token COLONCOLON;

           return (fun () ->  "::" ))

    <|> case "constr_ident_2" (perform

           token FALSE;

           return (fun () ->  "false" ))

    <|> case "constr_ident_3" (perform

           token TRUE;

           return (fun () ->  "true" ))

    <|> case "constr_ident_4" (perform

           v_1 <-- get_UIDENT;

           return (fun () ->  v_1 ))

      )

  method val_longident = rule "val_longident" (fun () -> dummy
    <|> case "val_longident_0" (perform

           v_1 <-- self#mod_longident ;
           token DOT;
           v_3 <-- self#val_ident ;

           return (fun () ->  Ldot(v_1, v_3) ))

    <|> case "val_longident_1" (perform

           v_1 <-- self#val_ident ;

           return (fun () ->  Lident v_1 ))

      )

  method constr_longident = rule "constr_longident" (fun () -> dummy
    <|> case "constr_longident_0" (perform

           token LBRACKET;
           token RBRACKET;

           return (fun () ->  Lident "[]" ))

    <|> case "constr_longident_1" (perform

           token LPAREN;
           token RPAREN;

           return (fun () ->  Lident "()" ))

    <|> case "constr_longident_2" (perform

           token FALSE;

           return (fun () ->  Lident "false" ))

    <|> case "constr_longident_3" (perform

           token TRUE;

           return (fun () ->  Lident "true" ))

    <|> case "constr_longident_4" (perform

           v_1 <-- self#mod_longident ;
           (* %prec below_DOT *)

           return (fun () ->  v_1 ))

      )

  method label_longident = rule "label_longident" (fun () -> dummy
    <|> case "label_longident_0" (perform

           v_1 <-- self#mod_longident ;
           token DOT;
           v_3 <-- get_LIDENT;

           return (fun () ->  Ldot(v_1, v_3) ))

    <|> case "label_longident_1" (perform

           v_1 <-- get_LIDENT;

           return (fun () ->  Lident v_1 ))

      )

  method type_longident = rule "type_longident" (fun () -> dummy
    <|> case "type_longident_0" (perform

           v_1 <-- self#mod_ext_longident ;
           token DOT;
           v_3 <-- get_LIDENT;

           return (fun () ->  Ldot(v_1, v_3) ))

    <|> case "type_longident_1" (perform

           v_1 <-- get_LIDENT;

           return (fun () ->  Lident v_1 ))

      )

  method mod_longident = leftrec "mod_longident" self#mod_longident_nonleftrec self#mod_longident_leftrec

  method mod_longident_nonleftrec = (dummy
    <|> case "mod_longident_nonleftrec_0" (perform

           v_1 <-- get_UIDENT;

           return (fun () ->  Lident v_1 ))

      )

  method mod_longident_leftrec v_1 = (dummy
    <|> case "mod_longident_leftrec_0" (perform

           token DOT;
           v_3 <-- get_UIDENT;

           return (fun () ->  Ldot(v_1, v_3) ))

      )

  method mod_ext_longident = leftrec "mod_ext_longident" self#mod_ext_longident_nonleftrec self#mod_ext_longident_leftrec

  method mod_ext_longident_nonleftrec = (dummy
    <|> case "mod_ext_longident_nonleftrec_0" (perform

           v_1 <-- get_UIDENT;

           return (fun () ->  Lident v_1 ))

      )

  method mod_ext_longident_leftrec v_1 = (dummy
    <|> case "mod_ext_longident_leftrec_0" (perform

           token LPAREN;
           v_3 <-- self#mod_ext_longident ;
           token RPAREN;

           return (fun () ->  lapply v_1 v_3 ))

    <|> case "mod_ext_longident_leftrec_1" (perform

           token DOT;
           v_3 <-- get_UIDENT;

           return (fun () ->  Ldot(v_1, v_3) ))

      )

  method mty_longident = rule "mty_longident" (fun () -> dummy
    <|> case "mty_longident_0" (perform

           v_1 <-- self#mod_ext_longident ; (* It may eat too much. Covered by the mty_longident_x *) 
           v_3 <-- option (perform 
                             token DOT;
                             self#ident) ;

           return (fun () ->  
             match v_3 with
             | Some v_3 -> Ldot(v_1, v_3)
             | None -> v_1))

    <|> case "mty_longident_1" (perform

           v_1 <-- self#ident ;

           return (fun () ->  Lident v_1 ))

      )

  method clty_longident = rule "clty_longident" (fun () -> dummy
    <|> case "clty_longident_0" (perform

           v_1 <-- self#mod_ext_longident ;
           token DOT;
           v_3 <-- get_LIDENT;

           return (fun () ->  Ldot(v_1, v_3) ))

    <|> case "clty_longident_1" (perform

           v_1 <-- get_LIDENT;

           return (fun () ->  Lident v_1 ))

      )

  method class_longident = rule "class_longident" (fun () -> dummy
    <|> case "class_longident_0" (perform

           v_1 <-- self#mod_longident ;
           token DOT;
           v_3 <-- get_LIDENT;

           return (fun () ->  Ldot(v_1, v_3) ))

    <|> case "class_longident_1" (perform

           v_1 <-- get_LIDENT;

           return (fun () ->  Lident v_1 ))

      )

  method toplevel_directive = rule "toplevel_directive" (fun () -> dummy
    <|> case "toplevel_directive_0" (perform

           token SHARP;

           (perform                                     
              v_2 <-- self#ident ;
              token FALSE;

              return (fun () ->  Ptop_dir(v_2, Pdir_bool false)))

           <|> (perform
                  v_2 <-- self#ident ;
                  v_3 <-- get_INT;

                  return (fun () ->  Ptop_dir(v_2, Pdir_int v_3) ))

           <|> (perform
                  v_2 <-- self#ident ;
                  v_3 <-- get_STRING;

                  return (fun () ->  Ptop_dir(v_2, Pdir_string v_3)))

           <|> (perform 
                  v_2 <-- self#ident ;
                  token TRUE;

                  return (fun () ->  Ptop_dir(v_2, Pdir_bool true) ))

           <|> (perform

                  v_2 <-- self#ident ;
                  v_3 <-- self#val_longident ;

                  return (fun () ->  Ptop_dir(v_2, Pdir_ident v_3) ))

           <|> (perform

                  v_2 <-- self#ident ;

                  return (fun () ->  Ptop_dir(v_2, Pdir_none) )))

      )

  method name_tag = rule "name_tag" (fun () -> dummy
    <|> case "name_tag_0" (perform

           token BACKQUOTE;
           v_2 <-- self#ident ;

           return (fun () ->  v_2 ))

      )

  method rec_flag = rule "rec_flag" (fun () -> dummy
    <|> case "rec_flag_0" (perform

           token REC;

           return (fun () ->  Recursive ))

    <|> case "rec_flag_1" (perform

           (* empty *)

           return (fun () ->  Nonrecursive ))

      )

  method direction_flag = rule "direction_flag" (fun () -> dummy
    <|> case "direction_flag_0" (perform

           token DOWNTO;

           return (fun () ->  Downto ))

    <|> case "direction_flag_1" (perform

           token TO;

           return (fun () ->  Upto ))

      )

  method private_flag = rule "private_flag" (fun () -> dummy
    <|> case "private_flag_0" (perform

           token PRIVATE;

           return (fun () ->  Private ))

    <|> case "private_flag_1" (perform

           (* empty *)

           return (fun () ->  Public ))

      )

  method mutable_flag = rule "mutable_flag" (fun () -> dummy
    <|> case "mutable_flag_0" (perform

           token MUTABLE;

           return (fun () ->  Mutable ))

    <|> case "mutable_flag_1" (perform

           (* empty *)

           return (fun () ->  Immutable ))

      )

  method virtual_flag = rule "virtual_flag" (fun () -> dummy
    <|> case "virtual_flag_0" (perform

           token VIRTUAL;

           return (fun () ->  Virtual ))

    <|> case "virtual_flag_1" (perform

           (* empty *)

           return (fun () ->  Concrete ))

      )

  method override_flag = rule "override_flag" (fun () -> dummy
    <|> case "override_flag_0" (perform

           token BANG;

           return (fun () ->  Override ))

    <|> case "override_flag_1" (perform

           (* empty *)

           return (fun () ->  Fresh ))

      )

  method opt_bar = rule "opt_bar" (fun () -> dummy
    <|> case "opt_bar_0" (perform

           token BAR;

           return (fun () ->  () ))

    <|> case "opt_bar_1" (perform

           (* empty *)

           return (fun () ->  () ))

      )

  method opt_semi = rule "opt_semi" (fun () -> dummy
    <|> case "opt_semi_0" (perform

           token SEMI;

           return (fun () ->  () ))

    <|> case "opt_semi_1" (perform

           (* empty *)

           return (fun () ->  () ))

      )

  method subtractive = rule "subtractive" (fun () -> dummy
    <|> case "subtractive_0" (perform

           token MINUS;

           return (fun () ->  "-" ))

    <|> case "subtractive_1" (perform

           token MINUSDOT;

           return (fun () ->  "-." ))

      )

  method additive = rule "additive" (fun () -> dummy
    <|> case "additive_0" (perform

           token PLUS;

           return (fun () ->  "+" ))

    <|> case "additive_1" (perform

           token PLUSDOT;

           return (fun () ->  "+." ))

      )

(* Oh, they are for my own modifs. Not existing in the vanilla ocaml.
  method locident = rule "locident" (fun () -> dummy
    <|> case "locident_0" (perform

           v_1 <-- self#mod_locident ;
           token DOT;
           v_3 <-- self#ident ;

           return (fun () ->  mklocident (LLdot(v_1, v_3)) ))

    <|> case "locident_1" (perform

           v_1 <-- self#ident ;

           return (fun () ->  mklocident (LLident v_1) ))

      )

  method mod_locident = leftrec "mod_locident" self#mod_locident_nonleftrec self#mod_locident_leftrec

  method mod_locident_nonleftrec = (dummy
    <|> case "mod_locident_nonleftrec_0" (perform

           v_1 <-- get_UIDENT;

           return (fun () ->  mklocident (LLident v_1) ))

      )

  method mod_locident_leftrec v_1 = (dummy
    <|> case "mod_locident_leftrec_0" (perform

           token LPAREN;
           v_3 <-- self#mod_locident ;
           token RPAREN;

           return (fun () ->  mklocident (LLapply(v_1, v_3)) ))

    <|> case "mod_locident_leftrec_1" (perform

           token DOT;
           v_3 <-- get_UIDENT;

           return (fun () ->  mklocident (LLdot(v_1, v_3)) ))

      )
*)

end
(* /rules *)

(* trailer *)

(* /trailer *)
