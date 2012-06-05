(******************************************************************************
 *                             Core-extended                                  *
 *                                                                            *
 * Copyright (C) 2008- Jane Street Holding, LLC                               *
 *    Contact: opensource@janestreet.com                                      *
 *    WWW: http://www.janestreet.com/ocaml                                    *
 *                                                                            *
 *                                                                            *
 * This library is free software; you can redistribute it and/or              *
 * modify it under the terms of the GNU Lesser General Public                 *
 * License as published by the Free Software Foundation; either               *
 * version 2 of the License, or (at your option) any later version.           *
 *                                                                            *
 * This library is distributed in the hope that it will be useful,            *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 * Lesser General Public License for more details.                            *
 *                                                                            *
 * You should have received a copy of the GNU Lesser General Public           *
 * License along with this library; if not, write to the Free Software        *
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                            *
 ******************************************************************************)

open Core.Std;;

(* Natural ordering like found in gnome nautilus, the mac finder etc...
   Refer to Mli for more documentation
*)

let collate s1 s2 =
  let pos1 = ref 0
  and pos2 = ref 0  in

  let next ~ok s pos =
    if (!pos) = String.length s then
      None
    else
      let c = s.[!pos] in
      if ok c then begin
        incr pos;
        Some c
      end else
        None
  in

  let compare_non_numerical () =
    let ok c = not (Char.is_digit c) in
    let rec loop () =
      match next ~ok s1 pos1,next ~ok s2 pos2 with
      | Some _, None -> 1
      | None , Some _ -> -1
      | None , None -> 0
      | Some c1,Some c2 when c1 = c2 -> loop ()
      | Some c1,Some c2 -> Char.compare c1 c2
    in
    loop ()
  in

  let compare_numerical () =
    let rec consume0 s pos  =
      match next ~ok:((=) '0') s pos with
      | Some _ -> consume0 s pos
      | None ->  ()
    in
     (* Our main loop works on string representation of ints where all the
        trailing zeros have been chopped of. Their magnitude is given by the
        length of their representation. If they have the same magnitude the
        lexical order is correct. Bias is used to save that information.
     *)
    let ok = Char.is_digit in
    let bias = ref 0 in
    let rec loop () =
      match next ~ok s1 pos1,next ~ok s2 pos2 with
      | Some _, None -> 1
      | None , Some _ -> -1
      | None , None when !bias <> 0-> !bias
      | None , None ->
          (* Both ints have the same value, The one with the shortest
             representation (i.e. the least trailing zeroes) is
             considered to be the smallest*)
          !pos1 - !pos2
      | Some c1,Some c2 when !bias = 0 ->
          bias := Char.compare c1 c2;
          loop ()
      | Some _ , Some _ -> loop ()
    in
    consume0 s1 pos1;
    consume0 s2 pos2;
    loop ()
  in

  let s1_length = String.length s1 in
  let s2_length = String.length s2 in
  let rec loop () =
    let r  = compare_non_numerical () in
    let r' = compare_numerical () in
    match r,r' with
    | 0,0 when !pos1 = s1_length && !pos2 = s2_length -> 0
    | 0,0 -> loop ()
    | 0,i | i,_ -> i
  in
  loop ()
;;

let concat_map ?sep ~f l =
  String.concat ?sep (List.map ~f l)

(**
   Inverse operation of [String.escaped]
*)
exception Unescape_error of bool*int*string

(* The stdlib's escaped does a lot of fancy wazoo magic to avoid
   using a buffer:
   It works in two passes, the first one calculates the length of the string to
   allocate and the second one does the actual escaping.

   This would be more cumbersome to do here but might be worth the hassle if
   performance ever gets to be an issue *)
let unescaped' ?(strict=true) s =
  let len = String.length s in
  let pos = ref 0 in
  let error ?(fatal=false) message =
    raise (Unescape_error (fatal,!pos,message))
  in
  let consume () =
    let i = !pos in
    if i = len then error "unexpectedly reached end of string";
    let c = s.[i] in
    pos := i + 1;
    c
  in
  let res = Buffer.create len in
  let emit c = Buffer.add_char res c in
  let emit_code code =
    match Char.of_int code with
    | Some c -> emit c
    | None -> error ~fatal:true
        (Printf.sprintf "got invalid escape code %d" code)
  in
  let rec loop () =
    if !pos < len then begin
      let c = consume () in
      if c <> '\\' then
        emit c
      else begin
        let mark = !pos in
        try
          let c = consume () in
          match c with
          | '\\' | '\"' -> emit c
          | 'b' -> emit '\b'
          | 'n' -> emit '\n'
          | 'r' -> emit '\r'
          | 't' -> emit '\t'
          | '\n' ->
              let rec consume_blank () =
                if !pos < len then begin
                  match consume () with
                  | ' ' | '\t' -> consume_blank ()
                  | _ -> decr pos
                end
              in
              consume_blank ()
          | 'x' ->
              let c2hex c =
                if (c >= 'A') && (c <= 'F' ) then
                  (Char.to_int c) + 10 - Char.to_int 'A'
                else if (c >= 'a') && (c <= 'f' ) then
                  (Char.to_int c) + 10 - Char.to_int 'a'
                else if (c >= '0') && (c <= '9') then
                  (Char.to_int c) - Char.to_int '0'
                else
                  error (Printf.sprintf "expected hex digit, got: %c" c);
              in
              let c1 = consume () in
              let c2 = consume () in
              emit_code (16 * c2hex c1 + c2hex c2);
          | c when Char.is_digit c ->
              let char_to_num c =
                match Char.get_digit c with
                | None -> error (Printf.sprintf "expected digit,got: %c" c);
                | Some i -> i
              in
              let i1 = char_to_num c in
              let i2 = char_to_num (consume ()) in
              let i3 = char_to_num (consume ()) in
              emit_code (100 * i1 + 10 * i2 + i3);
          | c -> error (Printf.sprintf "got invalid escape character: %c" c);
        with Unescape_error (false,_,_) when not strict ->
          emit '\\';
          pos := mark
      end;
      loop ()
    end else
      Buffer.contents res;
  in
  loop ();
;;

let unescaped ?strict s =
  try
    unescaped' ?strict s
  with Unescape_error (_,pos,message) ->
    invalid_argf "String.unescaped error at position %d of %s: %s"
      pos s message ()

let unescaped_res ?strict s =
  try
    Core.Result.Ok (unescaped' ?strict s)
  with Unescape_error (_,pos,message) ->
    Core.Result.Error (pos,message)


let squeeze str =
  let len = String.length str in
  let buf = Buffer.create len in
  let rec skip_spaces i =
    if i >= len then
      Buffer.contents buf
    else
      let c = str.[i] in
      if (c = ' ') || (c = '\n') || (c = '\t') || (c = '\r') then
        skip_spaces (i+1)
      else
        begin
          Buffer.add_char buf c;
          copy_chars (i+1)
        end
  and copy_chars i =
    if i >= len then
      Buffer.contents buf
    else
      let c = str.[i] in
      if (c = ' ') || (c = '\n') || (c = '\t') || (c = '\r') then
        begin
          Buffer.add_char buf ' ';
          skip_spaces (i+1)
        end
      else
        begin
          Buffer.add_char buf c;
          copy_chars (i+1)
        end
  in
  copy_chars 0
;;


let pad_right ?(char=' ') s l =
  let src_len = String.length s in
  if src_len >= l then
    s
  else
    let res = String.create l in
    String.blit ~src:s ~dst:res ~src_pos:0 ~dst_pos:0 ~len:src_len;
    String.fill ~pos:src_len ~len:(l-src_len) res char;
    res

let pad_left ?(char=' ') s l =
  let src_len = String.length s in
  if src_len >= l then
    s
  else
    let res = String.create l in
    String.blit ~src:s ~dst:res ~src_pos:0 ~dst_pos:(l-src_len) ~len:src_len;
    String.fill ~pos:0 ~len:(l-src_len) res char;
    res

let line_break ~len s =
  let buf = Buffer.create len in
  let flush_buf () =
    let res = Buffer.contents buf in
    Buffer.reset buf;
    res
  in
  let rec loop acc = function
    | [] ->
        let acc = if Buffer.length buf <> 0 then
          flush_buf ():: acc
        else if acc = [] then
          [""]
        else
          acc
        in
        List.rev acc
    | h::t when Buffer.length buf = 0 ->
        Buffer.add_string buf h;
        loop acc t
    | h::t when (Buffer.length buf + 1 + String.length h) < len ->
        Buffer.add_char buf ' ';
        Buffer.add_string buf h;
        loop acc t
    | l ->
        loop (flush_buf ()::acc) l
  in
  List.concat_map (String.split ~on:'\n' s)
    ~f:(fun s -> loop [] (String.split ~on:' ' s))

(* Finds out where to break a given line; returns the len of the line to break
    and the staring position of the next line.*)
let rec word_wrap__break_one
    ~hard_limit
    ~soft_limit
    ~previous_match
    s
    ~pos
    ~len =
  if pos = String.length s then
    len,pos
  else if previous_match > 0 && len >= soft_limit then
    previous_match,pos-len+previous_match+1
  else if len >= hard_limit then
    len,pos
  else
    match s.[pos] with
      (* Detect \r\n as one newline and not two... *)
    | '\r' when pos < String.length s -1 && s.[pos + 1] = '\n' ->
        len,pos+2
    | '\r' | '\n' -> len,pos+1
    | ' ' | '\t' ->
        word_wrap__break_one s
          ~hard_limit
          ~soft_limit
          ~previous_match:len
          ~pos:(pos+1)
          ~len:(len+1)
    | _ ->
        word_wrap__break_one s
          ~previous_match
          ~hard_limit
          ~soft_limit
          ~pos:(pos+1)
          ~len:(len+1)


(* Returns an pos*length list of all the lines (as substrings of the argument
   passed in) *)

let rec word_wrap__find_substrings ~hard_limit ~soft_limit s acc pos =
  if pos < String.length s then begin
    let len,new_pos = word_wrap__break_one s
      ~hard_limit
      ~soft_limit
      ~previous_match:0
      ~pos
      ~len:0
    in
    word_wrap__find_substrings
      ~hard_limit
      ~soft_limit
      s
      ((pos,len)::acc)
      new_pos
  end else
    acc

let word_wrap
    ?(trailing_nl=false)
    ?(soft_limit=80)
    ?(hard_limit=Int.max_value)
    ?(nl="\n") s =
  let soft_limit = min soft_limit hard_limit in
  let lines = word_wrap__find_substrings ~soft_limit ~hard_limit s [] 0 in
  match lines with
  | [] | [_] ->
      if trailing_nl then s^nl else s
  | ((hpos,hlen)::t) ->
      let nl_len = String.length nl in
      let body_len =
        List.fold_left t
          ~f:(fun acc (_,len) -> acc + nl_len + len)
          ~init:0
      in
      let res_len =
        if trailing_nl then body_len+hlen+nl_len else body_len+hlen
      in
      let res = String.create res_len in
      if trailing_nl then begin
        String.blit
          ~src:nl
          ~dst:res
          ~len:nl_len
          ~src_pos:0
          ~dst_pos:(body_len+hlen);
      end;
      String.blit
        ~src:s
        ~dst:res
        ~len:hlen
        ~src_pos:hpos
        ~dst_pos:body_len;
      let rec blit_loop dst_end_pos = function
        | [] -> ()
        | (src_pos,len)::rest ->
            let dst_pos = dst_end_pos-len-nl_len in
            String.blit
              ~src:s
              ~dst:res
              ~len
              ~src_pos
              ~dst_pos;
            String.blit
              ~src:nl
              ~dst:res
              ~len:nl_len
              ~src_pos:0
              ~dst_pos:(dst_pos + len);
            blit_loop dst_pos rest
      in
      blit_loop body_len t;
      res

(* Knuth-Morris-Pratt string matching. *)


let is_substring  ~substring t =
  let kmp_prefix len ~substring =
    let prefix = Array.create len 0 in
    let rec f ~k ~q =
      if q > len then prefix
      else (
        let k =
          let rec g k =
            if k <= 0 || ((String.get substring k) = (String.get substring (q - 1))) then k
            else g (prefix.(k - 1))
          in
          g k
        in
        let k =
          if (String.get substring k) = (String.get substring (q - 1))
          then k + 1 else k
        in
        assert (q - 1 >= 0 && q - 1 < len);
        Array.set prefix (q - 1) k;
        f ~k ~q:(q + 1)
      )
    in
    f ~k:0 ~q:2
  in
  let n = String.length t in
  let m = String.length substring in
  let prefix = kmp_prefix m ~substring in
  let rec f ~q ~i =
    if i > n then false
    else
    let q = (
      let q =
        let rec g q =
          if q <= 0 || ((String.get substring q) = (String.get t (i - 1)))
          then q
          else g (prefix.(q - 1))
        in
        g q
      in
      if String.get substring q = String.get t (i - 1) then q + 1
      else q
    )
    in
    if q = m then true
    else f ~q ~i:(i + 1)
  in
  f ~q:0 ~i:1

module Escaping = struct
  let escape_gen ~escapeworthy_map ~escape_char s =
    let get_code_for c =
      if c = escape_char then c else
        match List.Assoc.find escapeworthy_map c with
        | None -> failwith "escape_gen bug"
        | Some x -> x
    in
    let escapeworthy c =
      c = escape_char || List.exists ~f:(fun (c',_) -> c = c') escapeworthy_map in
    let count_escapeworthy_chars s =
      let ctr = ref 0 in
      for i = 0 to String.length s - 1 do
        if escapeworthy s.[i] then ctr := !ctr + 1
      done;
      !ctr
    in
    let really_escape_string count s =
      let ns = String.create count in
      let ns_pos = ref 0 in
      for i = 0 to String.length s - 1 do
        if escapeworthy s.[i] then
          begin
            ns.[!ns_pos] <- escape_char;
            ns.[!ns_pos + 1] <- get_code_for s.[i];
            ns_pos := !ns_pos + 2
          end
        else
          begin
            ns.[!ns_pos] <- s.[i];
            ns_pos := !ns_pos + 1
          end
      done;
      ns
    in
    let ewc = count_escapeworthy_chars s in
    if ewc > 0 then really_escape_string (ewc + String.length s) s
    else s


  let escape ~escapeworthy ~escape_char str =
    let escapeworthy = escape_char :: escapeworthy in
    let threshold = List.fold ~f:(fun m p -> if p < fst m then (p, snd m)
                                       else if p > snd m then (fst m, p) else m)
      ~init:(List.hd_exn escapeworthy, List.hd_exn escapeworthy) escapeworthy in
    let min = fst threshold in
    let max = snd threshold in
    let strlen = (String.length str) - 1 in
    let smark = ref 0
    and dmark = ref 0 in
    let newstr =  String.create ((strlen + 1) * 2) in
    let blit send =   (* copies smark to send into newstr *)
      let len = (send - !smark + 1) in
      String.blit ~src:str ~src_pos:!smark ~dst:newstr ~dst_pos:!dmark ~len:len;
      smark := send + 1;
      dmark := !dmark + len;
    in
    let rec exists lst c =
      match lst with
      | [] -> false
      | p :: rst -> if p = c then true else exists rst c
    in
    for i = 0 to strlen do
      if str.[i] >= min && str.[i] <= max &&
        List.exists ~f:(fun c -> c = str.[i]) escapeworthy then
          begin
            blit i;
            newstr.[!dmark - 1] <- escape_char;
            newstr.[!dmark] <- str.[i];
            dmark := !dmark + 1;
          end
    done;
    if !smark > 0 then
      begin
        if !smark <= strlen then blit strlen;
        String.sub newstr ~pos:0 ~len:!dmark
      end
    else
      str

  let escape_one_orig ~escapeworthy ~escape_char str =
    let len = String.length str in
    let rec loop cnt i =
      if i < 0 then cnt
      else
        let new_i = i - 1 in
        let c = str.[i] in
        if c = escapeworthy || c = escape_char then loop (cnt + 2) new_i
        else loop (cnt + 1) new_i
    in
    let len_1 = len - 1 in
    let cnt = loop 0 len_1 in
    if cnt = len then str
    else
      let res = String.create cnt in
      let rec loop src_pos dst_pos =
        if src_pos < 0 then res
        else
          let new_src_pos = src_pos - 1 in
          let c = str.[src_pos] in
          res.[dst_pos] <- c;
          if c = escapeworthy || c = escape_char then (
            res.[dst_pos - 1] <- escape_char;
            loop new_src_pos (dst_pos - 2))
          else loop new_src_pos (dst_pos - 1)
      in
      loop len_1 (cnt - 1)

  let escape_two_orig ~escapeworthy1 ~escapeworthy2 ~escape_char str =
    let len = String.length str in
    let rec loop cnt i =
      if i < 0 then cnt
      else
        let new_i = i - 1 in
        let c = str.[i] in
        if c = escapeworthy1 || c = escapeworthy2 || c = escape_char
        then loop (cnt + 2) new_i
        else loop (cnt + 1) new_i
    in
    let len_1 = len - 1 in
    let cnt = loop 0 len_1 in
    if cnt = len then str
    else
      let res = String.create cnt in
      let rec loop src_pos dst_pos =
        if src_pos < 0 then res
        else
          let new_src_pos = src_pos - 1 in
          let c = str.[src_pos] in
          res.[dst_pos] <- c;
          if c = escapeworthy1 || c = escapeworthy2 || c = escape_char then (
            res.[dst_pos - 1] <- escape_char;
            loop new_src_pos (dst_pos - 2))
          else loop new_src_pos (dst_pos - 1)
      in
      loop len_1 (cnt - 1)

  let unescape_gen ~map ~escape_char =
    let get_c_for_code code =
      if code = escape_char then code else
        match List.Assoc.find map code with
        | None -> code
        | Some x -> x
    in
    let count_escape_chars s =
      let ctr = ref 0 in
      let i = ref 0 in
      while !i < String.length s - 1 do
        if s.[!i] = escape_char then
          begin
            incr ctr;
            i := !i + 2
          end
        else
          i := !i + 1
      done;
      !ctr
    in
    let really_unescape_string num_escape_char os =
      let ns_length = String.length os - num_escape_char in
      let ns = String.create ns_length in
      let os_pos = ref 0 in
      for i = 0 to ns_length - 1 do
        if os.[!os_pos] = escape_char then
          begin
            ns.[i] <- get_c_for_code (os.[!os_pos + 1]);
            os_pos := !os_pos + 2;
          end
        else
          begin
            ns.[i] <- os.[!os_pos];
            os_pos := !os_pos + 1;
          end
      done;
      ns
    in
    (fun str ->
       let num_escape_chars = count_escape_chars str in
       if num_escape_chars > 0 then really_unescape_string num_escape_chars str
       else str)

  let unescape ~escape_char str = unescape_gen ~map:[] ~escape_char str

  (* this implementation is faster, if need be: *)
  (*
  let unescape ~escape_char str =
    let count_escape_chars s =
      let ctr = ref 0 in
      let i = ref 0 in
      while !i < String.length s - 1 do
        if s.[!i] = escape_char then
          begin
            incr ctr;
            i := !i + 2
          end
        else
          i := !i + 1
      done;
      !ctr
    in
    let really_unescape_string num_escape_char os =
      let ns_length = String.length os - num_escape_char in
      let ns = String.create ns_length in
      let os_pos = ref 0 in
      for i = 0 to ns_length - 1 do
        if os.[!os_pos] = escape_char then
          begin
            ns.[i] <- os.[!os_pos + 1];
            os_pos := !os_pos + 2;
          end
        else
          begin
            ns.[i] <- os.[!os_pos];
            os_pos := !os_pos + 1;
          end
      done;
      ns
    in
    let num_escape_chars = count_escape_chars str in
    if num_escape_chars > 0 then really_unescape_string num_escape_chars str
    else str
      *)

end
