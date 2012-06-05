open Lazylist
open Stream_intf

(* CR jfuruse: buf and buffer are lousy *)
type buf = {
  base : Sstring.t; (* underlying stream *)
  buf_pos : int; (* the abs position of the head of buf *) 
  rel_pos : int; (* from the buf head in bytes from 0 *)
  abs_pos : Position.File.t; (* from the first of the stream *)
}

let default_buf = { base = Sstring.default_null;
                    buf_pos = -1;
                    rel_pos = -1;
                    abs_pos = Position.File.none }

let position_of_buf buf = buf.abs_pos

module Extend(Base : sig
  include S
  with type elem = char
  and  type Pos.t = Position.File.t
  val create_attr : buf -> attr
  val buf : t -> buf
end) = struct

  open Base

  let bytes t = (buf t).buf_pos + (buf t).rel_pos
   
  let advance_char abs_pos = function
    | '\n' -> Position.File.add_newlines abs_pos 1
    | _ -> Position.File.add_columns abs_pos 1

  let advance_string abs_pos s =
    let slen = String.length s in
    let rec fold abs_pos = function
      | n when n = slen -> abs_pos
      | n -> fold (advance_char abs_pos (String.unsafe_get s n)) (n+1)
    in
    fold abs_pos 0

  let rec create : buf -> t = fun i ->
    match Sstring.peek i.base with
    | None -> null (create_attr i)
    | Some (elem, _, buf') ->
        if i.rel_pos >= String.length elem then begin
          (* If the position is out of the head *)
          create { i with base = buf'; 
                          buf_pos = i.buf_pos + String.length elem;
                          rel_pos = i.rel_pos - String.length elem }
        end else 
    	  let char = String.unsafe_get elem i.rel_pos in
    	  let abs_pos' = advance_char i.abs_pos char in
          let i' = { i with rel_pos = i.rel_pos + 1; abs_pos = abs_pos' } in
    	  lazy (cons_desc char (create_attr i) (create i'))

  let set_position : t -> Position.File.t -> t = fun t pos -> 
    create { (buf t) with abs_pos = pos }

  let substr t start_abs_pos len =
    let i = buf t in
    if i.abs_pos.Position.File.byte > start_abs_pos then failwith "Sbuffer.substr: start_abs_pos is over";
    let buffer = Buffer.create len in
    let rec substr stream stream_pos (* abs position of head of buf *) pos len =
      match Sstring.peek stream with
      | None -> failwith "Sbuffer.substr: end of stream"
      | Some (string, _, stream') ->
          (*
          Format.eprintf "substr.substr %d %d %d %s@." stream_pos pos len 
            (try String.sub string 0 40 with _ -> string);
          *)
          let len_string = String.length string in
          let stream_pos' = stream_pos + len_string in (* abs position of head of stream' *)
          if stream_pos' <= pos then substr stream' stream_pos' pos len (* [string] is before [pos] *)
          else 
            (* [string] contains [pos] *)
    	    let start = pos - stream_pos in
    	    let copy_len = min len (len_string - start) in
    	    Buffer.add_substring buffer string start copy_len;
    	    let len' = len - copy_len in
    	    if len' <= 0 then 
              (* [pos + len] is in [string]. End of substr *)
              let s = Buffer.contents buffer in
              s,
              { base = stream;
                buf_pos = stream_pos;
                rel_pos = start + len;
                abs_pos = advance_string i.abs_pos s }
            else substr stream' stream_pos' stream_pos' len'
    in
    let s, buf = substr i.base i.buf_pos start_abs_pos len in 
    assert (String.length s = len);
    s, create buf
  
  let takeWhile p t =
    let i = buf t in
    let buffer = Buffer.create 10 in
    let rec iter stream buf_pos rel_pos =
      let finish rel_pos =
        let s = Buffer.contents buffer in
        s,
        { base = stream;
          buf_pos = buf_pos;
          rel_pos = rel_pos;
          abs_pos = advance_string i.abs_pos s }
      in
      match Sstring.peek stream with
      | None -> finish rel_pos
      | Some (string, _, stream') ->
          let len_string = String.length string in
          let rec iter_internal = function
            | n when len_string = n -> 
                (* try on the next string *)
                iter stream' (buf_pos + len_string) 0 
            | n ->
                let char = String.unsafe_get string n in
                if p char then begin
                  Buffer.add_char buffer char;
                  iter_internal (n+1)
                end else finish n
          in
          iter_internal rel_pos
    in
    let s, buf = iter i.base i.buf_pos i.rel_pos in 
    s, create buf
  
  let from_string ~filename str = 
    create { base = Sstring.from_string str;
             buf_pos = 0;
             rel_pos = 0;
             abs_pos = Position.File.top filename
           }
  
  let from_chan ~filename ic = 
    create { base = Sstring.from_chan ic;
             buf_pos = 0;
             rel_pos = 0;
             abs_pos = Position.File.top filename
           }
end
