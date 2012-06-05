open Planck
open Printf
open Token

open Input.Parser

module From_OCaml_source_code = struct
  (* Identical or almost identical code *)

  (* The table of keywords *)
  
  let keyword_table =
    let tbl = Hashtbl.create 149 in
    List.iter (fun (k,v) -> Hashtbl.add tbl k v) [
      "and", AND;
      "as", AS;
      "assert", ASSERT;
      "begin", BEGIN;
      "class", CLASS;
      "constraint", CONSTRAINT;
      "do", DO;
      "done", DONE;
      "downto", DOWNTO;
      "else", ELSE;
      "end", END;
      "exception", EXCEPTION;
      "external", EXTERNAL;
      "false", FALSE;
      "for", FOR;
      "fun", FUN;
      "function", FUNCTION;
      "functor", FUNCTOR;
      "if", IF;
      "in", IN;
      "include", INCLUDE;
      "inherit", INHERIT;
      "initializer", INITIALIZER;
      "lazy", LAZY;
      "let", LET;
      "match", MATCH;
      "method", METHOD;
      "module", MODULE;
      "mutable", MUTABLE;
      "new", NEW;
      "object", OBJECT;
      "of", OF;
      "open", OPEN;
      "or", OR;
      "private", PRIVATE;
      "rec", REC;
      "sig", SIG;
      "struct", STRUCT;
      "then", THEN;
      "to", TO;
      "true", TRUE;
      "try", TRY;
      "type", TYPE;
      "val", VAL;
      "virtual", VIRTUAL;
      "when", WHEN;
      "while", WHILE;
      "with", WITH;
  
      "mod", INFIXOP3("mod");
      "land", INFIXOP3("land");
      "lor", INFIXOP3("lor");
      "lxor", INFIXOP3("lxor");
      "lsl", INFIXOP4("lsl");
      "lsr", INFIXOP4("lsr");
      "asr", INFIXOP4("asr")
    ];
    tbl

  (* Remove underscores from float literals *)
  
  let remove_underscores s =
    let l = String.length s in
    let rec remove src dst =
      if src >= l then
        if dst >= l then s else String.sub s 0 dst
      else
        match String.unsafe_get s src with
          '_' -> remove (src + 1) dst
        |  c  -> String.unsafe_set s dst c; remove (src + 1) (dst + 1)
    in remove 0 0

  let char_for_backslash = function
    | 'n' -> '\010'
    | 'r' -> '\013'
    | 'b' -> '\008'
    | 't' -> '\009'
    | c   -> c

  (* CR jfuruse: need to be reset at the lexing
     (first of all, is this side effect ok in planck?) *)
  let in_comment = ref false

  let char_for_decimal_code pos c1 c2 c3 =
    let c = 100 * (Char.code c1 - 48) +
             10 * (Char.code c2 - 48) +
                  (Char.code c3 - 48) in
    if (c < 0 || c > 255) then
      if !in_comment
      then return 'x'
      else raise (Critical_error (pos, "illegal escape"))
    else return (Char.chr c)
  
  let char_for_hexadecimal_code c1 c2 =
    let d1 = Char.code c1 in
    let val1 = if d1 >= 97 then d1 - 87
      else if d1 >= 65 then d1 - 55
      else d1 - 48
    in
    let d2 = Char.code c2 in
    let val2 = if d2 >= 97 then d2 - 87
      else if d2 >= 65 then d2 - 55
      else d2 - 48
    in
    Char.chr (val1 * 16 + val2)
end

include From_OCaml_source_code

let zero = token '0'

let underscore = token '_'

let decimal_char : char t = 
  tokenp (function
    | '0' .. '9' -> true
    | _ -> false)
  <?> "decimal"

let bin_char : char t = one_of ['0'; '1']

let oct_char : char t = 
  tokenp (function
    | '0' .. '7' -> true
    | _ -> false) 
    <?> "octal"

let hex_char : char t =
  tokenp (function
    | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' -> true
    | _ -> false)
    <?> "hex"

let char_or_underscores : 'a t -> string t = fun f ->
  matched (perform
             ignore f;
             ?* (ignore f <|> underscore))

let decimal_literal = char_or_underscores decimal_char

let bin_literal : string t = perform
  matched (perform
    zero;
    ignore (one_of ['b'; 'B']);
    ignore (char_or_underscores bin_char))

let oct_literal : string t = perform
  matched (perform
    zero;
    ignore (one_of ['o'; 'O']);
    ignore (char_or_underscores oct_char))

let hex_literal : string t = perform
  matched (perform
    zero;
    ignore (one_of ['x'; 'X']);
    ignore (char_or_underscores hex_char))

let int_literal = (hex_literal <!> bin_literal <!> oct_literal) </> decimal_literal

let int = perform
  pos <-- position;
  s <-- int_literal;
  match try Some (int_of_string s) with Failure _ -> None with
  | Some n -> return (INT n)
  | None -> error "int literal overflow" <?@> pos

(*
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
*)
(* CR jfuruse: it is not regexp like. No backtracking... *)
let float_literal = perform
  matched (perform
             ignore (char_or_underscores decimal_char);
             option_ (perform
                        token '.';
                        option_ (ignore (char_or_underscores decimal_char)));
             option_ (seq_ [ ignore (one_of ['e'; 'E']);
                             option_ (ignore (one_of ['-'; '+']));
                             ignore (char_or_underscores decimal_char) ]))

let float = perform
  str <-- float_literal;
  return (FLOAT (remove_underscores str))

let int32 = perform
  pos <-- position;
  s <-- int_literal;
  token 'l';
  try
    return (INT32 (Int32.of_string s))
  with Failure _ -> critical_error pos "int32 literal overflow"

let int64 = perform
  pos <-- position;
  s <-- int_literal;
  token 'L';
  try
    return (INT64 (Int64.of_string s))
  with Failure _ -> critical_error pos "int64 literal overflow"

let nativeint = perform
  pos <-- position;
  s <-- int_literal;
  token 'n';
  try
    return (NATIVEINT (Nativeint.of_string s))
  with Failure _ -> critical_error pos "nativeint literal overflow"

let newline = string "\r\n" <|> ignore (one_of ['\n'; '\r'])
let blank = ignore (one_of [' '; '\009'; '\012'])
let lowercase = tokenp (function
  | 'a'..'z' | '\223'..'\246' | '\248'..'\255' | '_' -> true
  | _ -> false) <?> "lowercase char"
let uppercase = tokenp (function
  | 'A'..'Z' | '\192'..'\214' | '\216'..'\222' -> true
  | _ -> false) <?> "uppercase char"
let is_identchar = function
  | 'A'..'Z' | 'a'..'z' | '_' | '\192'..'\214' | '\216'..'\246' | '\248'..'\255' | '\'' | '0'..'9' -> true
  | _ -> false
let identchar = tokenp is_identchar <?> "ident char"
let is_symbolchar = function
  | '!' | '$' | '%' | '&' | '*' | '+' | '-' | '.' | '/' | ':' | '<' | '=' | '>' | '?' | '@' | '^' | '|' | '~' -> true
  | _ -> false
let symbolchar = tokenp is_symbolchar

let rec string_internal pos buf = 
  (perform
     token '"';
     return (String.concat "" (List.rev buf)))
  <|> (* '\\' case *)
      (perform
         pos <-- position;
         token '\\';
       
         (perform 
            newline;
            ??* (function ' ' | '\t' -> true | _ -> false);
            string_internal pos buf)

         <|> (perform
               c <-- token_result (function
                 | '\\' -> Result.Ok '\\'
                 | '\'' -> Result.Ok '\''
                 | '"' ->  Result.Ok '\"'
                 | 'n' ->  Result.Ok '\n'
                 | 't' ->  Result.Ok '\t'
                 | 'b' ->  Result.Ok '\b'
                 | 'r' ->  Result.Ok '\r'
                 | ' ' ->  Result.Ok '\ '
                 | _ -> Result.Error "Illegal escape char");
               string_internal pos (String.make 1 c :: buf))

         <|> (perform
                c1 <-- decimal_char;
                c2 <-- decimal_char;
                c3 <-- decimal_char;
                c <-- char_for_decimal_code pos c1 c2 c3; (* CR jfuruse: error is never reported! *)
                string_internal pos (String.make 1 c :: buf))

         (* Need backtrack since \3 is still a valid string (with a warning, though) *)
            
         <!> (perform
                token 'x';
                c1 <-- hex_char;
                c2 <-- hex_char;
                string_internal pos (String.make 1 (char_for_hexadecimal_code c1 c2) :: buf))

         <!> (perform
                c <-- take;
                if !in_comment then string_internal pos buf
                else begin
                  (* CR jfuruse: TODO *)
                  (* Location.prerr_warning loc Warnings.Illegal_backslash; *)
                  string_internal pos (("\\" ^ String.make 1 c) :: buf)
                end) )

  <|> (perform
         s <-- matched newline;
         string_internal pos (s :: buf))

  <|> (perform
         eos;
         critical_error pos "unterminated string")

  <|> (perform
         s <-- matched (?+ (tokenp (function '"' | '\\' -> false | _ -> true)));
         string_internal pos (s :: buf))

(* string in OCaml but string is used in Planck *)
let string_ = perform
  pos <-- position;
  token '"';
  s <-- string_internal pos [];
  return (STRING s)

let char_internal = 
  (perform
     s <-- matched newline;
     return (String.unsafe_get s 0) (* Funny that only the first char is used *))
  <|> tokenp (function '\\' | '\'' | '\010' | '\013' -> false
                     | _ -> true)
  <|> (* '\\' case *)
      (perform 
         pos <-- position;
         token '\\';

         (perform
            c <-- one_of ['\\'; '\''; '"'; 'n'; 't'; 'b'; 'r'; ' '];
            return (char_for_backslash c))
         <|> (perform 
                c1 <-- decimal_char;
                c2 <-- decimal_char;
                c3 <-- decimal_char;
                char_for_decimal_code pos c1 c2 c3)
         <|> (perform
                token 'x';
                c1 <--hex_char;
                c2 <-- hex_char;
                return (char_for_hexadecimal_code c1 c2)) 
         <|> error "illegal escape")

let char = perform
  token '\'';
  c <-- char_internal;
  token '\'';
  return (CHAR c)

let rec comment levs = perform
  pos <-- position;
  string "(*";
  \ in_comment := true;
  comment_internal (pos :: levs)

and comment_internal levs = 
  comment levs

  <!> (perform
         ignore (string "*)");
         (* CR jfuruse: it's side effective and may not work! *)
         match levs with
         | [] -> assert false (* since it has seen at least one beginning of comment! *)
         | [_] -> in_comment := false; return ()
         | _ :: lev' -> comment_internal lev')

  <!> (perform
         pos <-- position;
         token '"';
         ignore (string_internal pos []); (* CR jfuruse; the result of string_internal is useless... 
                                             concatenation could be outside of string_internal *)
         (* CR jfuruse: error handling is not done
            (EOS is done but no check of nice string in comment thing)
          with Error (Unterminated_string, _) ->
            match !comment_start_loc with
            | [] -> assert false
            | loc :: _ -> comment_start_loc := [];
                          raise (Error (Unterminated_string_in_comment, loc))
         *)
         comment_internal levs)
  <!> (perform
         string "''";
         comment_internal levs)
  <!> (perform
         token '\'';
         newline;
         token '\'';
         comment_internal levs)
  <!> (perform
         token '\'';
         ignore (tokenp (function
           | '\\' | '\'' | '\010' | '\013' -> false
           | _ -> true));
         token '\'';
         comment_internal levs)
  <!> (perform
         token '\'';
         ignore (one_of ['\\'; '"'; '\''; 'n'; 't'; 'b'; 'r'; ' ']);
         token '\'';
         comment_internal levs)
  <!> (perform
         token '\'';
         ignore decimal_char;
         ignore decimal_char;
         ignore decimal_char;
         token '\'';
         comment_internal levs)
  <!> (perform
         string "\\x";
         ignore hex_char;
         ignore hex_char;
         token '\'';
         comment_internal levs)

  <!> (perform
         eos;
         match levs with
         | [] -> assert false
         | loc::_ -> critical_error loc "unterminated comment")

  <!> (perform
         newline;
         comment_internal levs)
  <!> (perform
         ignore take;
         comment_internal levs)

let comment () = comment []

let lident = matched (lowercase >>= fun _ -> ??* is_identchar)
let uident = matched (uppercase >>= fun _ -> ??* is_identchar)

(* token in OCaml. ``token'' is already used as Planck.token *) 
let rec ocaml_token () : (Token.t * Str.Pos.t) t = perform
  start_pos <-- position;

  let with_pos m = m >>= fun v -> return (v, start_pos) in
  let return_with_pos v = return (v, start_pos) in

  (eos >>= fun () -> return_with_pos EOF)

  <|> (?+ (blank <|> newline) >>= ocaml_token)

  <|> (* including case of '_' *)
      (lident >>= function
         | "_" -> return_with_pos UNDERSCORE
         | s -> return_with_pos (try Hashtbl.find keyword_table s with Not_found -> LIDENT s))

  <|> (uident >>= fun s -> return_with_pos (UIDENT s))

  <|> (* case of ~ *)
      ((perform
          token '~';
          name <-- lident;
          token ':';
          if Hashtbl.mem keyword_table name then error "keyword as label" (* CR jfuruse: not reported! *)
          else return_with_pos (LABEL name))
          
       <!> (matched (perform token '~'; ??* is_symbolchar) >>= function
              | "~" -> return_with_pos TILDE
              | s -> return_with_pos (PREFIXOP s)))

  <|> (* Include the case of '.' *)
      (with_pos (int </> float </> int32 </> int64 </> nativeint)
          
        (* the above may fail consuming '.' *)

        (* case of . *)
       <!> ( (string ".." >>= fun () -> return_with_pos DOTDOT)
             <|> (string "."  >>= fun () -> return_with_pos DOT) ) )

  <|> with_pos string_

  <|> (* case of '\'' *)
      (with_pos char
       <!> (token '\'' >>= fun () -> return_with_pos QUOTE))

  <|> (* case of '(' *)
      ( ((perform   (* must come previous than [comment ()] *)
            pos <-- position;
            string "(*)";
            return pos)
         (* after printing a warnig, continue parsing as a comment *)   
         <&> (fun _pos -> perform
                (* CR jfuruse: TODO
                   Location.prerr_warning loc Warnings.Comment_start; *)
                \ prerr_endline "Warning around (*)";
                comment ();
                ocaml_token ()))

       <!> (comment () >>= ocaml_token)

       <!> (token '('  >>= fun () -> return_with_pos LPAREN) )


  <|> (* case of '*' *)
      (((perform
           pos <-- position;
           string "*)";
           return pos)
        <&> (fun _pos -> perform
               (* CR jfuruse: TODO
                  Location.prerr_warning loc Warnings.Comment_not_end; *)
               \ prerr_endline "Warning comment not end";
               token '*';
               return_with_pos STAR))

       <!> (matched (token '*' >>= fun _ -> ignore (takeWhile  is_symbolchar)) >>= function
              | "*" -> return_with_pos STAR
              | s ->
                  if String.unsafe_get s 1 = '*' then return_with_pos (INFIXOP4 s) (* ** case *)
                  else return_with_pos (INFIXOP3 s)))

  <|> (* case of '#' *)
      ((perform
          token '#';
          ??* (function ' ' | '\t' -> true | _ -> false);
          num <-- matched (?+ decimal_char);
          ??* (function ' ' | '\t' -> true | _ -> false);
          name_opt <-- option (perform
                                 token '"';
                                 name <-- ??** (function '\010' | '\013' | '"' -> false
                                                       | _ -> true);
                                 token '"';
                                 return name);
          ??* (function '\010' | '\013' -> false
                      | _ -> true);
          newline;
 
          (* CR jfuruse: not tested well.. *)
          (* set the stream position *)
          (* update_loc lexbuf name (int_of_string num) true 0; *)
          pos <-- position;
          str <-- stream;
          let name = match name_opt with Some name -> name | None -> pos.Position.File.fname in
          let str' = Input.Stream.set_position str { pos with Position.File.fname = name; line = int_of_string num; column = 0 } in
          set_stream str';
          ocaml_token ())
       <!> (token '#'  >>= fun () -> return_with_pos SHARP)) 

  <|> (* case of '?' *)
      (perform
         token '?';
         ( (perform
              name <-- lident;
              token ':';
              if Hashtbl.mem keyword_table name then error "keyword as label" (* CR jfuruse: not reported! *)
              else return_with_pos (OPTLABEL name))
           (* We need the following backtrack for ?label (without :) *)
           <!> (??** is_symbolchar >>= 
                  function
                    | "" -> return_with_pos QUESTION
                    | "?" -> return_with_pos QUESTIONQUESTION
                    | s -> return_with_pos (PREFIXOP ("?" ^ s)))))

  (* CR jfuruse: Should be extremely slow *)      
  <|> (
       (matched (token '&' >>= fun () -> ??* is_symbolchar) >>= 
          function
            | "&&" -> return_with_pos AMPERAMPER
            | "&" -> return_with_pos AMPERSAND
            | s -> return_with_pos (INFIXOP0 s))

       <|> (matched (token '-' >>= fun () -> ??* is_symbolchar) >>=
              function
                | "-" ->  return_with_pos MINUS
                | "-." ->  return_with_pos MINUSDOT
                | "->" ->  return_with_pos MINUSGREATER
                | s -> return_with_pos (INFIXOP2 s))

       <|> (token '`'  >>= fun () -> return_with_pos BACKQUOTE)
       <|> (token ','  >>= fun () -> return_with_pos COMMA)
       <|> (token ')'  >>= fun () -> return_with_pos RPAREN)

       <|> (string "|]" >>= fun () -> return_with_pos BARRBRACKET)
       <|> (matched (token '|' >>= fun _ -> ??* is_symbolchar) >>=
              function
                | "|" -> return_with_pos BAR
                | "||" -> return_with_pos BARBAR
                | s -> return_with_pos (INFIXOP0 s))

       <|> (string "::" >>= fun () -> return_with_pos COLONCOLON)
       <|> (string ":=" >>= fun () -> return_with_pos COLONEQUAL)
       <|> (string ":>" >>= fun () -> return_with_pos COLONGREATER)
       <|> (token ':'  >>= fun () -> return_with_pos COLON)

       <|> (string ";;" >>= fun () -> return_with_pos SEMISEMI)
       <|> (token ';'  >>= fun () -> return_with_pos SEMI)

       <|> (matched (token '<' >>= fun _ -> ??* is_symbolchar) >>=
              function
                | "<-" -> return_with_pos LESSMINUS
                | "<" -> return_with_pos LESS
                | s -> return_with_pos (INFIXOP0 s))

       <|> (matched (token '=' >>= fun () -> ??* is_symbolchar) >>=
              function
                | "=" -> return_with_pos EQUAL
                | s -> return_with_pos (INFIXOP0 s))

       <|> (string "[|" >>= fun () -> return_with_pos LBRACKETBAR)
       <|> (string "[<" >>= fun () -> return_with_pos LBRACKETLESS)
       <|> (string "[>" >>= fun () -> return_with_pos LBRACKETGREATER)
       <|> (string "["  >>= fun () -> return_with_pos LBRACKET)

       <|> (string "]"  >>= fun () -> return_with_pos RBRACKET)

       <|> (string "{<" >>= fun () -> return_with_pos LBRACELESS)
       <|> (string "{"  >>= fun () -> return_with_pos LBRACE)

       <|> (string ">]" >>= fun () -> return_with_pos GREATERRBRACKET)
       <|> (string ">}" >>= fun () -> return_with_pos GREATERRBRACE)
       <|> (matched (token '>' >>= fun () -> ??* is_symbolchar) >>=
              function
                | ">" -> return_with_pos GREATER
                | s -> return_with_pos (INFIXOP0 s))

       <|> (string "}"  >>= fun () -> return_with_pos RBRACE)

       <|> (matched (token '!' >>= fun () -> ??* is_symbolchar) >>=
              function
                | "!" -> return_with_pos BANG
                | "!=" -> return_with_pos (INFIXOP0 "!=")
                | s -> return_with_pos (PREFIXOP s))

       <|> (matched (token '+' >>= fun () -> ??* is_symbolchar) >>=
              function
                | "+" -> return_with_pos PLUS
                | "+." -> return_with_pos PLUSDOT
                | s -> return_with_pos (INFIXOP2 s))

       <|> (matched (token '$' >>= fun () -> ??* is_symbolchar) 
            >>= fun s -> return_with_pos (INFIXOP0 s))
       <|> (matched (one_of ['@'; '^'] >>= fun _ -> ??* is_symbolchar) 
            >>= fun s -> return_with_pos (INFIXOP1 s))
       <|> (matched (one_of ['/'; '%'] >>= fun _ -> ??* is_symbolchar) 
            >>= fun s -> return_with_pos (INFIXOP3 s)))

  <|> (take >>= fun c -> error (sprintf "ocaml lexer: illegal character %C" c) <?@> start_pos)

(* Make region *)
let ocaml_token = perform
  (v, pos_start) <-- ocaml_token ();
  pos_end <-- position;
  let reg = {Position.Region.start = pos_start; end_ = pos_end } in
  return (v, reg)

let skip_sharp_bang = 
  (perform
     string "#!";
     ??* ((<>) '\n');
     token '\n';
     ??* ((<>) '\n');
     string "\n!#\n";
     return ())
  <!> (perform 
         string "#!";
         ??* ((<>) '\n');
         token '\n';
         return ())
  <!> return ()

let ocaml_token_stream = Token.Stream.create (perform
  (v, pos) <-- ocaml_token;
  match v with
  | EOF -> return (None, pos)
  | _ -> return (Some v, pos))
