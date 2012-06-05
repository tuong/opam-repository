open Planck
open Sexplib.Conv

type t =
  | AMPERAMPER
  | AMPERSAND
  | AND
  | AS
  | ASSERT
  | BACKQUOTE
  | BANG
  | BAR
  | BARBAR
  | BARRBRACKET
  | BEGIN
  | CHAR of (char)
  | CLASS
  | COLON
  | COLONCOLON
  | COLONEQUAL
  | COLONGREATER
  | COMMA
  | CONSTRAINT
  | DO
  | DONE
  | DOT
  | DOTDOT
  | DOWNTO
  | ELSE
  | END
  | EOF
  | EQUAL
  | EXCEPTION
  | EXTERNAL
  | FALSE
  | FLOAT of (string)
  | FOR
  | FUN
  | FUNCTION
  | FUNCTOR
  | GREATER
  | GREATERRBRACE
  | GREATERRBRACKET
  | IF
  | IN
  | INCLUDE
  | INFIXOP0 of (string)
  | INFIXOP1 of (string)
  | INFIXOP2 of (string)
  | INFIXOP3 of (string)
  | INFIXOP4 of (string)
  | INHERIT
  | INITIALIZER
  | INT of (int)
  | INT32 of (int32)
  | INT64 of (int64)
  | LABEL of (string)
  | LAZY
  | LBRACE
  | LBRACELESS
  | LBRACKET
  | LBRACKETBAR
  | LBRACKETLESS
  | LBRACKETGREATER
  | LESS
  | LESSMINUS
  | LET
  | LIDENT of (string)
  | LPAREN
  | MATCH
  | METHOD
  | MINUS
  | MINUSDOT
  | MINUSGREATER
  | MODULE
  | MUTABLE
  | NATIVEINT of (nativeint)
  | NEW
  | OBJECT
  | OF
  | OPEN
  | OPTLABEL of (string)
  | OR
  | PLUS
  | PLUSDOT
  | PREFIXOP of (string)
  | PRIVATE
  | QUESTION
  | QUESTIONQUESTION
  | QUOTE
  | RBRACE
  | RBRACKET
  | REC
  | RPAREN
  | SEMI
  | SEMISEMI
  | SHARP
  | SIG
  | STAR
  | STRING of (string)
  | STRUCT
  | THEN
  | TILDE
  | TO
  | TRUE
  | TRY
  | TYPE
  | UIDENT of (string)
  | UNDERSCORE
  | VAL
  | VIRTUAL
  | WHEN
  | WHILE
  | WITH
with sexp

let equal a b = 
  let a_int = Obj.is_int (Obj.repr a) in
  let b_int = Obj.is_int (Obj.repr b) in
  if a_int && b_int then (Obj.magic a : int) = (Obj.magic b : int)
  else if not a_int && not b_int then
    match a, b with
    | CHAR a, CHAR b -> a = b
    | FLOAT a, FLOAT b -> a = b
    | INFIXOP0 a, INFIXOP0 b -> a = b
    | INFIXOP1 a, INFIXOP1 b -> a = b
    | INFIXOP2 a, INFIXOP2 b -> a = b
    | INFIXOP3 a, INFIXOP3 b -> a = b
    | INFIXOP4 a, INFIXOP4 b -> a = b
    | INT a, INT b -> a = b
    | INT32 a, INT32 b -> a = b
    | INT64 a, INT64 b -> a = b
    | LABEL a, LABEL b -> a = b
    | LIDENT a, LIDENT b -> a = b
    | NATIVEINT a, NATIVEINT b -> a = b
    | OPTLABEL a, OPTLABEL b -> a = b
    | PREFIXOP a, PREFIXOP b -> a = b
    | STRING a, STRING b -> a = b
    | UIDENT a, UIDENT b -> a = b
    | _ -> false
  else false

type _token = t
let show t = Sexplib.Sexp.to_string_hum (sexp_of_t t)

module Stream = struct
  open Lazylist

  module MemoKey = struct
    type t = string
    let hash = Hashtbl.hash
    let equal (x : string) y = x = y
  end

  module Memo = Hashtbl.Make(MemoKey)

  module Base = struct
    type elem = t
    let show_elem = show
    let equal_elem = equal
    module Pos = Position.Region
    type attr = Pos.t option (* last consumed token position *) * Pos.t * (Obj.t, exn) Result.t Memo.t
    let position_of_attr (_,pos,_) = pos
    let last_position_of_attr (last_pos,_,_) = last_pos 
    let default_attr = None, Pos.none, Memo.create 1
    let memo_of_attr (_,_,memo) = memo
    let buf_of_attr (_,buf,_) = buf
  end

  module Str = Stream.Make(Base)

  include Str

  include Smemo.Extend(struct
    include Str
    module Memo = Memo
    let memo st = Base.memo_of_attr (attr st)
  end)

  (* CR jfuruse: generalize it and port back to Planck.Core *)
  let create (m : ('a option * Position.Region.t) Input.Parser.t) = fun st ->
    let rec f last_pos st = lazy begin
      match Input.Parser.run m st with
      | Result.Ok ((None, pos), _st') -> null_desc (last_pos, pos, Memo.create 107) (* EOS case *)
      | Result.Ok ((Some v, pos), st') -> cons_desc v (last_pos, pos, Memo.create 107) (f (Some pos) st')
      | Result.Error (pos, s) -> raise (Input.Parser.Critical_error (pos, s))
    end
    in
    f None st
  ;;

  let last_position st : Position.Region.t option = Base.last_position_of_attr (attr st)
end

module Parser = struct
  include Pbase.Make(Stream)
  let last_position : Position.Region.t t = perform
    st <-- stream;
    return (match Stream.last_position st with
      | Some reg -> reg
      | None -> Position.Region.none)

  (** Efficient version of with_region *)
  let with_region (t : 'a t) : ('a * Position.Region.t) t = perform
    last_bot_pos <-- last_position;
    last_top_pos <-- position;
    res <-- t;
    top_pos <-- position;
    bot_pos <-- last_position;
    let pos = 
      if last_top_pos = top_pos then 
        (* No advancement. OCaml's behaviour is: return the end of the last consumed position *)
        { last_bot_pos with Position.Region.start = last_bot_pos.Position.Region.end_ }
      else
        { Position.Region.start = last_top_pos.Position.Region.start; end_ = bot_pos.Position.Region.end_ }
    in
    (* \ assert (Position.Region.is_valid pos); *)
    return (res, pos)
end
