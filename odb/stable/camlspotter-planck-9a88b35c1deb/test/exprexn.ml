open Spotlib.Spot
open Planck

(* Stream of chars with buffering and memoization *)
module Stream = struct

  (* The configuration of the stream *)    
  module Base = struct

    (* Stream elements *)
    type elem = char (* It is a char stream *)
    let show_elem = Printf.sprintf "%C" (* How to pretty print the element *)

    let equal_elem (x : char) y = x = y
    (* Stream attributes *)
    type attr = Sbuffer.buf (* Stream elements carry internal buffers *)
    let default_attr = Sbuffer.default_buf

    (* Stream positions *)
    module Pos = Position.File (* Type of the element position *)
    let position_of_attr attr = Sbuffer.position_of_buf attr (* How to obtain the position from attr *)
  end

  module Str = Stream.Make(Base) (* Build the stream *)
  include Str

  (* Extend Str with buffering *)
  include Sbuffer.Extend(struct
    include Str
    let create_attr buf = buf (* How to create an attribute from a buffer *)
    let buf st = attr st (* How to obtain the buffer of a stream *)
  end)

end

module Parser = struct

  module Base = Pbaseexn.Make(Stream) (* Base parser *)
  include Base

  include Pbuffer.Extend(Stream)(Base) (* Extend Base with parser operators for buffered streams *)
end    

open Parser (* open Parser namespace *)

open Op_prec (* open Op_prec, functions for operator precedences *)

let _ = 
  Hashtbl.replace_list Op_prec.tbl [
    "+",  { prec = 2.0; kind = `Infix `Left };
    "-",  { prec = 2.0; kind = `Infix `Left };
    "*",  { prec = 3.0; kind = `Infix `Left };
    "/",  { prec = 3.0; kind = `Infix `Left };
    "~",  { prec = 5.0; kind = `Prefix }; (* unary minus *)
  ]
  
(* parsing rules *)
let blank = ignore (one_of [' '; '\t'; '\n'; '\r'])

let combine_leftrec (non_leftrec : 'a Parser.t) (leftrec : 'a -> 'a Parser.t) =
  
  non_leftrec >>= fun left ->
  
  let rec leftrecs left =
    (leftrec left >>= fun left' -> leftrecs left')
    <|> return left
  in
  leftrecs left

(* Eta expansions with [st] are required, unfortunatelly *)
let rec expr st = begin

  combine_leftrec expr_non_leftrec expr_leftrec

end st

and expr_non_leftrec st = begin (* constant, parened and unary minus *)  

  (* Skip spaces *)
  ?* blank >>= fun () -> 

  (constant >>= fun sv -> return (Op_prec.terminal sv))

  <|> (token '(' >>= fun () ->
       ?* blank >>= fun () ->
       expr >>= fun e ->
       ?* blank >>= fun () ->
       token ')' <?> "missing closing paren" >>= fun () ->
       return (Op_prec.parened (fun (s,v) -> Printf.sprintf "(%s)" s, v) e))
      
  (* Unary minus *)      
  <|> (token '-' >>= fun () ->
       ?* blank >>= fun () ->
       expr >>= fun e ->
       return (prefix "~" (fun (s,v) -> Printf.sprintf "~ %s" s, -v) e))

end st

and expr_leftrec e_left st = begin (* binop expr *)

  ?* blank >>= fun () ->

  (binop >>= fun binop ->
   ?* blank >>= fun () ->
   expr >>= fun e_right ->
   return (binop e_left e_right))

end st

and binop st = begin
  
  (token '+' >>= fun () ->
   return (Op_prec.infix "+" (fun (s1,v1) (s2,v2) -> Printf.sprintf "<%s + %s>" s1 s2, v1 + v2)))

  <|> (token '-' >>= fun () ->
       return (Op_prec.infix "-" (fun (s1,v1) (s2,v2) -> Printf.sprintf "<%s - %s>" s1 s2, v1 - v2)))

  <|> (token '*' >>= fun () ->
       return (Op_prec.infix "*" (fun (s1,v1) (s2,v2) -> Printf.sprintf "<%s * %s>" s1 s2, v1 * v2)))

  <|> (token '/' >>= fun () ->
       return (Op_prec.infix "/" (fun (s1,v1) (s2,v2) -> Printf.sprintf "<%s / %s>" s1 s2, v1 / v2)))

end st

and constant st = begin
  (* [0-9]+ *)
  (matched (?+ (tokenp (function '0'..'9' -> true | _ -> false) <?> "decimal")) >>= fun s -> 
   return (s, int_of_string s))

end st

(* For test *)

let rec random size = 
  let key = if size = 0 then 0 else Random.int 6 in
  match key with
  | 0 -> string_of_int (Random.int 10)
  | 1 -> "- " ^ random (size-1) 
  | 2 -> "(" ^ random (size-1) ^ ")"
  | 3 -> random (size-1) ^ " + " ^ random (size-1)
  | 4 -> random (size-1) ^ " - " ^ random (size-1)
  | 5 -> random (size-1) ^ " * " ^ random (size-1)
  | _ -> assert false

let test s = 
  Format.eprintf "input=%S@." s;
  let stream = Stream.from_string ~filename:"stdin" s in
  match fst ((result expr) stream) with
  | Result.Ok res -> 
      (* Check whether the original and parsed are identical *)
      (* Check of computed values are done outside of this program. See OMakefile. *)
      let str, v = build res in 
      let str_check = 
        let buf = Buffer.create (String.length str) in
        for i = 0 to String.length str - 1 do
          match str.[i] with
          | '~' -> Buffer.add_char buf '-'
          | '<' | '>' -> ()
          | c -> Buffer.add_char buf c
        done;
        Buffer.contents buf
      in
      Format.eprintf "parsed=%S@.res=%d@.@." str v;
      if s <> str_check then failwithf "FAILURE\n%s\n%s\n" s str_check;
      Format.printf "assert (%s = %d);;@." s v;
  | Result.Error (pos, s) ->
      Format.eprintf "%a: syntax error: %s@." Position.File.format pos s;
      raise Exit

let _ = 
  for i = 0 to 100 do
    test (random 20)
  done;

