open Token 
open Planck
module Location = Xlocation
open Location

open Token.Parser

let level = ref 0
let do_debug = ref false

let memoize = Token.Stream.memoize 

(* Non memoization: it IS REALLY SLOW *)
(* let memoize _k f v = f v *)

let dump_rhs_tbl rhs_counter rhs_tbl =
  Format.eprintf "%s   DUMP BEGIN@." (String.make (!level*2) ' ');
  for i = 1 to !rhs_counter-1 do
    Format.eprintf "%s   %d : %a@."
      (String.make (!level*2) ' ') 
      i
      Planck.Position.Region.format 
       (try Hashtbl.find rhs_tbl i with Not_found -> Position.Region.none);
  done;
  Format.eprintf "%s   END@." (String.make (!level*2) ' ') 

let dump_rhs_tbl () =
  match !rhs_tbl_stack with
  | [] -> assert false
  | (rhs_counter, rhs_tbl, _) :: _ -> dump_rhs_tbl rhs_counter rhs_tbl

(* CR jfuruse: Eeek! Side effects require eta! *)
let with_rhs (name : string) (t : ('a * Position.Region.t) Token.Parser.t) : 'a Token.Parser.t = fun st -> Profile.incr (); (perform
  let cntr = push_rhs_tbl name in
  (* with_region misses the first location, if the first rule consumes nothing *)
  res <-- result t;
  match res with
  | Result.Ok (v, rhs_loc) -> 
      pop_rhs_tbl cntr;
      (* add the result reg to the parent *)
      if !rhs_tbl_stack <> [] then add_rhs_pos rhs_loc; (* At the toplevel, stack is empty *)
      return v
  | Result.Error err -> pop_rhs_tbl cntr; throw err
  ) st

(* [method name = rule "name" (fun () -> perform 

                     case (perform
          
                         v_1 <-- self#symbol;
                         ... )
 
                     <|> case (perform

                         ... 

    )]

   The lambda abstraction is required. 
   Without it, self#name immediately calls the first rule at each case e.g. self#symbol.
   If #symbol is defined #name mutual left recursively, it loops forever. 
*)

let rule (name : string) (m : unit -> ('a * Position.Region.t) Token.Parser.t) : 'a Token.Parser.t = 
  (* This eta is required to avoid inf loop and segfault by stack overflow *) 
  fun st -> with_rhs name (memoize name (m ())) st

let get_poses () = 
  match !rhs_tbl_stack with
  | [] -> assert false
  | (rhs_counter, rhs_tbl, _) :: _ -> 
      if !do_debug then begin
        Format.eprintf "%s  get_poses@." (String.make (!level*2) ' ');
        dump_rhs_tbl ();
      end;
      let rec fold st i =
        if i = 0 then st
        else fold (Hashtbl.find rhs_tbl i :: st) (i-1)
      in
      fold [] (!rhs_counter - 1)

let case (name : string) (t : (unit -> 'a) Token.Parser.t) : ('a * Planck.Position.Region.t) Token.Parser.t = fun st -> 
  Profile.incr ();
  if !do_debug then prerr_endline (String.make (!level*2) ' ' ^ "-> " ^ name);
  let st_start = if !do_debug then Some st else None in (* Make [st] GCed as early as possible *) 

  incr level;

  let t = perform
    last_pos <-- last_position; (* bad for nonleftrec... *)
    f <-- t;
    let poses = get_poses () in
    (* action require symbol_rloc_ref is set *)
    let symbol_rloc = calc_symbol_rloc last_pos poses in 
    \ symbol_rloc_ref := symbol_rloc;
    return (f (), 
            calc_rhs_loc last_pos poses)
  in

  (result t >>= fun res ->
   
   decr level;

   match res with
   | Result.Ok (_, rhs_loc as v) -> 
       if !do_debug then begin perform
         st' <-- stream;
         \ begin match st_start with
         | None -> assert false
         | Some st -> 
             let elts = List.map fst (Token.Stream.between st st') in
             Format.eprintf "%s<- %s : %s@."
               (String.make (!level*2) ' ') 
               name 
               (Sexplib.Sexp.to_string_hum (Sexplib.Conv.sexp_of_list Token.sexp_of_t elts));
             Format.eprintf "%s   symbol_rloc: %a@."
               (String.make (!level*2) ' ') 
              Planck.Position.Region.format !symbol_rloc_ref;
             Format.eprintf "%s   rhs_loc:     %a@."
               (String.make (!level*2) ' ') 
               Planck.Position.Region.format rhs_loc;
             match !rhs_tbl_stack with
             | [] -> assert false
             | (rhs_counter, rhs_tbl, _) :: _ ->
                 for i = 1 to !rhs_counter-1 do
                   Format.eprintf "%s   %d : %a@."
                     (String.make (!level*2) ' ') 
                     i
                     Planck.Position.Region.format 
                     (try Hashtbl.find rhs_tbl i with Not_found -> Position.Region.none);
                 done;
                 Format.eprintf "%s   END@." (String.make (!level*2) ' ') 
         end;
         return v
       end else return v

  | Result.Error err ->
      if !do_debug then prerr_endline (String.make (!level*2) ' ' ^ "X- " ^ name);
      throw err

  ) st
 
let leftrec name =
  let name_leftrec_entry = name ^ "_leftrec_entry" in
  let name_nonleftrec = name ^ "_nonleftrec" in
  let name_leftrec = name ^ "_leftrec" in

  fun (nonleftrec_rule : ('a * Position.Region.t) t) (leftrec_rule : 'a -> ('a * Position.Region.t) t) ->
    rule name_leftrec_entry (fun () -> 
      case name_leftrec_entry (perform
        nonleftrec <-- with_rhs name_nonleftrec nonleftrec_rule;
        let nonleftrec_reg = 
          let (_, rhs_tbl, _) = List.hd !rhs_tbl_stack in
          Hashtbl.find rhs_tbl 1
        in
        let rec loop (v : 'a) reg = perform
          vopt <-- option (with_rhs name_leftrec 
                             (* We need ETA expansion ... sigh. *)
                             (fun st -> 
                               add_rhs_pos reg;
                               leftrec_rule v st));
          match vopt with
          | None -> return v
          | Some v' -> 
              let (rhs_counter, rhs_tbl, _) = List.hd !rhs_tbl_stack in
              let reg = Hashtbl.find rhs_tbl (!rhs_counter - 1) in
              loop v' reg
        in
        res <-- loop nonleftrec nonleftrec_reg;
        return (fun () -> res)))

(* overriding monad operators with position updates *)

(* only works for 'one token parsers' *)
let with_position m = perform
  pos <-- position;
  res <-- m;
  \ if !rhs_tbl_stack <> [] then add_rhs_pos pos;
  return res

let token v = with_position (token v)
let token_result m = with_position (token_result m) (* used for get_XXXX *)
let one_of tks = with_position (one_of tks)
let eos = with_position eos
let take = with_position take
let take_ = with_position take_
  
let dummy = error "dummy" (* to have <|> for each real rule *)

open Planck.Op_prec

let infixop0 = { prec = 0.0; kind = `Infix `Left }
let infixop1 = { prec = 1.0; kind = `Infix `Right }
let infixop2 = { prec = 2.0; kind = `Infix `Left }
let infixop3 = { prec = 3.0; kind = `Infix `Left }
let infixop4 = { prec = 4.0; kind = `Infix `Right }


let _ = 
  let old_find = !Planck.Op_prec.find in
  Planck.Op_prec.find := fun op ->
    match op with
    | ""          -> assert false
    | "<-"        -> { prec = -8.0; kind = `Noassoc }
    | ":="        -> { prec = -7.0; kind = `Infix `Right }
    | "as"        -> { prec = -6.0; kind = `Noassoc }
    | "|"         -> { prec = -5.0; kind = `Infix `Left }
    | ","         -> { prec = -4.0; kind = `Infix `Left }
    | "->"        -> { prec = -3.0; kind = `Infix `Right }
    | "or" | "||" -> { prec = -2.0; kind = `Infix `Right }
    | "&" | "&&"  -> { prec = -1.0; kind = `Infix `Right }

    | "::"        -> { prec = 1.5; kind = `Infix `Right }

    | "!=" -> infixop0 
    | "mod" | "land" | "lor" | "lxor" -> infixop3
    | "**" | "lsl" | "lsr" | "asr" -> infixop4

    | "prec_unary_minus" | "prec_unary_plus" -> { prec = 5.0  ; kind = `Noassoc }
    | "prec_constant_constructor"            -> { prec = 6.0  ; kind = `Noassoc }
    | "prec_constr_appl"                     -> { prec = 7.0  ; kind = `Noassoc }
    | "below_SHARP"                          -> { prec = 8.0  ; kind = `Noassoc }
    | "#"                                    -> { prec = 9.0  ; kind = `Noassoc }
    | "below_DOT"                            -> { prec = 10.0 ; kind = `Noassoc }
    | "."                                    -> { prec = 11.0 ; kind = `Noassoc }
    | "!" | "`"                              -> { prec = 12.0 ; kind = `Noassoc }
(*
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc BACKQUOTE BANG BEGIN CHAR FALSE FLOAT INT INT32 INT64
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LIDENT LPAREN
          NEW NATIVEINT PREFIXOP STRING TRUE UIDENT
*)
    | _ -> 
        match String.unsafe_get op 0 with
        | '!' | '~' | '?' -> { prec = 100.0; kind = `Prefix }
        | '=' | '<' | '>' | '|' | '&' | '$' -> infixop0
        | '@' | '^' -> infixop1
        | '+' | '-' -> infixop2
        | '*' | '/' -> infixop3
        | _ -> old_find op
