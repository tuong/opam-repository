(* File: cgi_common.ml

   Objective Caml Library for writing (F)CGI programs.

   Copyright (C) 2005

     Christophe Troestler
     email: Christophe.Troestler@umh.ac.be
     WWW: http://www.umh.ac.be/math/an/software/

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation, with the
   special exception on linking described in file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*)
(* 	$Id: cgi_common.ml,v 1.10 2006/01/08 22:19:31 chris_77 Exp $	 *)

open Cgi_types


let rev_split =
  let rec split_next acc i0 i1 delim s =
    if i1 = String.length s then
      (String.sub s i0 (i1 - i0)) :: acc
    else if String.unsafe_get s i1 = delim then
      split_next ((String.sub s i0 (i1 - i0)) :: acc) (i1+1) (i1+1) delim s
    else
      split_next acc i0 (i1 + 1) delim s  in
  split_next [] 0 0

let is_prefix =
  let rec is_pre i len pre s =
    if i < len then
      (String.unsafe_get pre i = String.unsafe_get s i)
      && is_pre (i+1) len pre s
    else true in
  fun prefix s ->
    (String.length prefix <= String.length s)
    && is_pre 0 (String.length prefix) prefix s


(* URL encoding and decoding
 **********************************************************************
 * See RFC 2396.
 *)

(* Decoding *)

exception Hex_of_char

let hex_of_char =
  let code_a = Char.code 'a' - 10
  and code_A = Char.code 'A' - 10 in
  function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> Char.code c - code_a
  | 'A' .. 'F' as c -> Char.code c - code_A
  | _ -> raise Hex_of_char


let rec decode_range_loop i0 i up s =
  if i0 >= up then i else begin
    match String.unsafe_get s i0 with
    | '+' ->
	String.unsafe_set s i ' ';
	decode_range_loop (succ i0) (succ i) up s
    | '%' when i0 + 2 < up ->
        let i1 = succ i0 in
        let i2 = succ i1 in
        let i0_next =
          try
            let v = hex_of_char(String.unsafe_get s i1) lsl 4
                    + hex_of_char(String.unsafe_get s i2) in
	    String.unsafe_set s i (Char.chr v);
	    succ i2
          with Hex_of_char ->
	    String.unsafe_set s i '%';
	    i1 in
	decode_range_loop i0_next (succ i) up s
    | c ->
	String.unsafe_set s i c;
	decode_range_loop (succ i0) (succ i) up s
  end

let is_space c =  c = ' ' || c = '\t' || c = '\r' || c = '\n'

(* [rm_htspace s] returns the substring [s.[low .. up - 1]] stripped
   of heading and trailing spaces. *)
let rm_htspace =
  let rec trailing_spaces j s = (* assume there is i s.t. s.[i] <> ' ' *)
    if is_space(String.unsafe_get s j) then trailing_spaces (pred j) s
    else j in
  let rec rm_spaces i up s =
    if i >= up then "" else begin
      if is_space(String.unsafe_get s i) then rm_spaces (succ i) up s
      else
        (* s.[i] <> space so trailing_spaces will stop and return j >= i. *)
        String.sub s i (trailing_spaces (pred up) s + 1 - i)
    end in
  fun low up s -> rm_spaces low up s

(* We strip heading and trailing spaces of key-value data (even though
   it does not conform the specs) because certain browsers do it, so
   the user should not rely on them.
   See e.g. https://bugzilla.mozilla.org/show_bug.cgi?id=114997#c6 *)
let decode_range s low up =
  if low >= up then "" else
    let up = decode_range_loop low low up s in
    rm_htspace low up s
    (* String.sub s low (up - low) *)



(* Encoding *)

(* Use a table lookup for speed. *)
let char_of_hex =
  let hex = [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9';
	       'A'; 'B'; 'C'; 'D'; 'E'; 'F' |] in
  fun i -> Array.unsafe_get hex i


let encode_wrt is_special s0 =
  let len = String.length s0 in
  let encoded_length = ref len in
  for i = 0 to len - 1 do
    if is_special(String.unsafe_get s0 i) then
      encoded_length := !encoded_length + 2
  done;
  let s = String.create !encoded_length in
  let rec do_enc i0 i = (* copy the encoded string in s *)
    if i0 < len then begin
      let s0i0 = String.unsafe_get s0 i0 in
      if is_special s0i0 then begin
        let c = Char.code s0i0 in
        let i1 = succ i in
        let i2 = succ i1 in
        String.unsafe_set s i '%';
        String.unsafe_set s i1 (char_of_hex (c lsr 4));
        String.unsafe_set s i2 (char_of_hex (c land 0x0F));
        do_enc (succ i0) (succ i2)
      end
      else if s0i0 = ' ' then begin
	String.unsafe_set s i '+';
        do_enc (succ i0) (succ i)
      end
      else begin
        String.unsafe_set s i s0i0;
        do_enc (succ i0) (succ i)
      end
    end in
  do_enc 0 0;
  s


(* Unreserved characters consist of all alphanumeric chars and the
   following limited set of punctuation marks and symbols: '-' | '_' |
   '.' | '!' | '~' | '*' | '\'' | '(' | ')'.  According to RFC 2396,
   they should not be escaped unless the context requires it. *)
let special_rfc2396 = function
  | ';' | '/' | '?' | ':' | '@' | '&' | '=' | '+' | '$' | ',' (* Reserved *)
  | '\000' .. '\031' | '\127' .. '\255' (* Control chars and non-ASCII *)
  | '<' | '>' | '#' | '%' | '"'         (* delimiters *)
  | '{' | '}' | '|' | '\\' | '^' | '[' | ']' | '`' (* unwise *)
      -> true
  | _ -> false
(* ' ' must also be encoded but its encoding '+' takes a single char. *)

let encode = encode_wrt special_rfc2396


let special_rfc2068 = function
  | '\000' .. '\031' | '\127' .. '\255' (* Control chars and non-ASCII *)
  | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"'
  | '/' | '[' | ']' | '?' | '=' | '{' | '}' (* tspecials *)
  | '+' | '%' (* not in RFC2068 but they serve to encode *)
      -> true
  | _ -> false
(* ' ' must also be encoded but its encoding '+' takes a single char. *)

let encode_cookie = encode_wrt special_rfc2068


(* Query parsing
 ***********************************************************************)
(* We assume that the query string is just a temporary handle of the
   key-val pairs so may be overwritten -- but we don't want to copy it
   as it may be large.  So the decoding is optimized to be in-place.
   The key-val pairs found are added to the hash [h]. *)

let rec get_key qs i0 i up h =
  if i >= up then Hashtbl.add h (decode_range qs i0 up) "" else
    match String.unsafe_get qs i with
    | '=' -> get_val qs (i+1) (i+1) up (decode_range qs i0 i) h
    | '&' ->
	Hashtbl.add h (decode_range qs i0 i) ""; (* key but no val *)
	get_key qs (i+1) (i+1) up h
    | _ ->
	get_key qs i0 (i+1) up h
and get_val qs i0 i up key h =
  if i >= up then Hashtbl.add h key (decode_range qs i0 up) else
    match String.unsafe_get qs i with
    | '&' ->
	Hashtbl.add h key (decode_range qs i0 i);
	get_key qs (i+1) (i+1) up h
    | _ -> get_val qs i0 (i+1) up key h

(* [parse_query_range qs low up h] adds the key-val pairs found in
   [qs.[low .. up-1]] to the hash [h]. *)
let parse_query_range qs low up h =
  get_key qs low low up h

let parse_query qs h =
  if qs <> "" then get_key qs 0 0 (String.length qs) h


(* Knuth-Morris-Pratt algorithm
 ***********************************************************************)

let preprocess pat m =
  let b = Array.make (m + 1) (-1) in
  (* [b.(i)] = width of  the widest border of [pat.[0 .. i-1]]. *)
  let j = ref(-1) in
  for i = 0 to m-1 do
    while !j >= 0 && String.unsafe_get pat !j <> String.unsafe_get pat i do
      j := Array.unsafe_get b !j
    done;
    incr j;
    Array.unsafe_set b (i+1) !j
  done;
  b


(* [search pat s i0 i1] search the string [pat] in [s.[i0 .. i1-1]]
   and return the position of the first match.
   @raise Not_found if [pat] is not found. *)
(* We favored the following imperative code because it is the fastest. *)
exception Found of int

let search pat =
  let m = String.length pat in
  let b = preprocess pat m in
  fun s i0 i1 ->
    let i = ref i0
    and j = ref 0 in
    try
      while !i < i1 do
	while !j >= 0 && String.unsafe_get s !i <> String.unsafe_get pat !j do
	  j := Array.unsafe_get b !j
	done;
	incr i;
	incr j;
	if !j = m then raise(Found(!i - !j))
      done;
      raise Not_found
    with Found i -> i


(* [search_case_fold pat s i0 i1] does the same as [search pat s i0
   i1] but in a case insensitive manner. *)
let search_case_fold pat =
  let m = String.length pat in
  let pat = String.lowercase pat in
  let b = preprocess pat m in
  fun s i0 i1 ->
      let i = ref i0
    and j = ref 0 in
    try
      while !i < i1 do
	while !j >= 0 && Char.lowercase(String.unsafe_get s !i)
	  <> String.unsafe_get pat !j do
	  j := Array.unsafe_get b !j
	done;
	incr i;
	incr j;
	if !j = m then raise(Found(!i - !j))
      done;
      raise Not_found
    with Found i -> i

(* RFC 2045 and RFC 822 values
 ***********************************************************************)

(* value ::= token | quoted-string         ; RFC2045
 * token ::= 1*<any US-ASCII CHAR except SPACE, CTLs, tspecials>
 *
 * quoted-string ::= '"' *(qtext|quoted-pair) '"'
 * qtext ::= <any CHAR except '"', '\\', CR and including linear-white-space>
 * linear-white-space ::= 1*([CRLF] (SPACE|HTAB))     ; folding
 * quoted-pair ::= '\\' CHAR
 *)

let special_rfc2045 = function
  | '\000' .. '\031' | '\127' .. '\255' (* CTLs and non-ASCII (e.g. '\t') *)
  | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"'
  | '/' | '[' | ']' | '?' | '='         (* tspecials *)
  | ' ' -> true
  | _ -> false

let rec get_token s i1 i =
  if i = i1 || special_rfc2045(String.unsafe_get s i)
  then i
  else get_token s i1 (succ i)

(* ',' allowed in boundary but unlikely -- disabled for MSIE bug, see
   [get_boundary] *)
let rec get_token_boundary s i1 i =
  if i = i1 || (let c = String.unsafe_get s i in special_rfc2045 c || c = ',')
  then i
  else get_token_boundary s i1 (succ i)


(* [get_quoted_string s i0 i1 i j] get the string starting at [i0]
   (i.e. s.[i0-1] = '"') and un-escape it.  [s.[i0 .. i1-1]] may be
   overwritten. *)
let rec get_quoted_string s i1 i j =
  if i = i1 then raise Not_found;
  match String.unsafe_get s i with
  | '"' -> j
  | '\\' ->
      let i' = succ i in
      if i' = i1 then raise Not_found;
      String.unsafe_set s j (String.unsafe_get s i');
      get_quoted_string s i1 (succ i') (succ j)
  | '\r' ->
      (* Check if folding *)
      let i' = succ i in
      let i'' = succ i' in
      if i'' < i1 && String.unsafe_get s i' = '\n'
	&& (let c = String.unsafe_get s i'' in c = ' ' || c = '\t') then begin
	  String.unsafe_set s j ' '; (* folding equiv SPACE *)
	  skip_folding s i1 (succ i'') (succ j)
	end
      else raise Not_found;
  | c ->
      (* qtext *)
      String.unsafe_set s j c;
      get_quoted_string s i1 (succ i) (succ j)

and skip_folding s i1 i j =
  if i > i1 then raise Not_found;
  (* We should check that ['\r'] is followed by ['\n' SPACE] but we
     are permissive... *)
  match String.unsafe_get s i with
  | ' ' | '\t' | '\r' | '\n' -> skip_folding s i1 (succ i) j
  | _ -> get_quoted_string s i1 i j


let get_value s i0 i1 =
  if String.unsafe_get s i0 = '"'
  then
    let i0 = succ i0 in
    let i = get_quoted_string s i1 i0 i0 in
    String.sub s i0 (i - i0)
  else
    String.sub s i0 ((get_token s i1 i0) - i0)

let get_value_boundary s i0 i1 =
  if String.unsafe_get s i0 = '"'
  then
    let i0 = succ i0 in
      let i = ref(get_quoted_string s i1 i0 i0) in
      (* Remove trailing spaces *)
      while !i >= i0
	&& (let c = String.unsafe_get s !i in c = ' ' || c = '\t')
      do decr i done;
      String.sub s i0 (!i - i0)
  else
    String.sub s i0 ((get_token_boundary s i1 i0) - i0)



(* multipart/form-data parsing (defined by RFC 2388)
 ***********************************************************************)

(* MSIE: with SSL connections, it sometimes produces CONTENT_TYPE like
   "multipart/form-data; boundary=..., multipart/form-data; boundary=..."
   Thus we have disabled ',' in [get_token_boundary] above.
 *)
let get_boundary =
  let bd = "boundary=" in
  let lbd = String.length bd in
  let bd = search_case_fold bd in
  fun content_type ->
    try
      let endbd = bd content_type 0 (String.length content_type) + lbd in
      get_value_boundary content_type endbd (String.length content_type)
    with
      Not_found -> ""

(* Headers of the different parts *)

let get_name =
  let srch = search_case_fold "name=" in
  fun s i0 i1 ->
    try
      let end_name = srch s i0 i1 + 5 (* length "name=" *) in
      get_value s end_name i1
    with
      Not_found -> ""

(* @raise Not_found if a filename cannot be found.

   Note: Most browsers send filenames without properly escaping them
   (e.g. "file\"\n") and, in these cases, the filename returned by
   this function may be different from the one actually uploaded. *)
let get_filename =
  let srch = search_case_fold "filename=" in
  fun s i0 i1 ->
    let end_filename = srch s i0 i1 + 9 (* length "filename=" *) in
    get_value s end_filename i1


(* If there is no "filename", then the default Content-Type is
   "text/plain" but we do not care because in this case the content
   will be treated as a param and not a file.  *)
let get_content_type =
  let srch = search_case_fold "Content-Type:" in
  fun s i0 i1 ->
    try
      let i = ref(srch s i0 i1 + 13 (* "Content-Type:" *) ) in
      (* Skip space *)
      while !i < i1
	&& (let c = String.unsafe_get s !i in c = ' ' || c = '\t')
      do incr i done;
      let i0 = !i in
      i := get_token s i1 !i;  (* First token *)
      if !i < i1 && String.unsafe_get s !i = '/' then
	i := get_token s i1 (!i + 1);  (* Second token *)
      String.lowercase(String.sub s i0 (!i - i0))
    with
      Not_found -> "application/octet-stream"


let get_content_transfer_encoding =
  let srch = search_case_fold "Content-Transfer-Encoding:" in
  fun s i0 i1 ->
    try
      let i = ref(srch s i0 i1 + 26 (* "Content-Transfer-Encoding:" *) ) in
      (* Skip space *)
      while !i < i1
	&& (let c = String.unsafe_get s !i in c = ' ' || c = '\t')
      do incr i done;
      String.lowercase(get_value s !i i1)
    with
      Not_found -> ""


let search_crlf = search "\r\n"
let search_2crlf = search "\r\n\r\n"
let search_2dash = search "--"

(* The multipart is structured as:

   --boundary
   <header lines>

   <body>
   --boundary
   ...
   --boundary--

   All the lines are terminated with "\r\n", so the "empty line" is
   exactly "\r\n".  The [boundary] is given in the CONTENT-TYPE
   meta-variable.

   [parse_multipart boundary data request] parses [data] and fill the
   appropriate [request] fields.
   @raise Not_found if an expected pattern is not found. *)
let parse_multipart boundary data request =
  let len_data = String.length data in
  let boundary, part1 =
    if boundary = "" then
      (* It seems older versions of Netscape are unreliable about
	 providing a boundary string.  We try to determine it from
	 [data] by looking for "--" followed by a token. *)
      let i0 = search_2dash data 0 (String.length data) in
      let i1 = ref i0 in
      while !i1 < len_data
	&& not(special_rfc2045(String.unsafe_get data !i1)) do incr i1 done;
      let i2 = search_crlf data !i1 len_data in
      (String.sub data i0 (!i1 - i0), i2)
    else
      let bd = "--" ^ boundary in
      let i0 = search bd  data 0 len_data in
      let i2 = search_crlf data (i0 + String.length bd) len_data in
      (bd, i2)  in
  let search_bd = search ("\r\n" ^ boundary) in
  let len_bd = 2 (* \r\n *) + String.length boundary in

  let begin_part = ref part1 in
  try
    while true do
      (* Get head information (if any) *)
      let end_head = search_2crlf data !begin_part len_data in
      (* FIXME: Should we restrict the search for "name=" and
	 "filename=" to the "Content-Disposition:" line and be more
	 careful they do not apprear in values? *)
      let name = get_name data !begin_part end_head in
      let filename, is_file =
	try (get_filename data !begin_part end_head, true)
	with Not_found -> ("", false) in
      let content_type = get_content_type data !begin_part end_head in
      let encoding = get_content_transfer_encoding data !begin_part end_head in
      (* Get data and add it to the request *)
      let d0 = end_head + 4 (* \r\n\r\n *) in
      let d1 = search_bd data d0 len_data in
      let value = String.sub data d0 (d1 - d0) in
      if is_file then
	Hashtbl.add request.uploads name {
	  upload_value = value;
	  upload_filename = filename;
	  upload_content_type = content_type;
	  (* encoding *)
	}
      else
	Hashtbl.add request.params name value;
      (* Is it the closing boundary? *)
      let end_bd = d1 + len_bd in
      if end_bd + 2 <= len_data && String.unsafe_get data end_bd = '-'
	  && String.unsafe_get data (end_bd + 1) = '-' then
	    raise Exit;
      (* Skip the boundary line and get the next part *)
      begin_part := search_crlf data end_bd len_data
    done
  with
    Exit -> ()



exception Unsupported_media_type of string

(* [is_prefix_ci p s] chekw whether [p] is a prefix of [s] in a case
   insensitive way.  [p] is assumed to be lowercase. *)
let is_prefix_ci p s =
  let lp = String.length p in
  (lp <= String.length s) && (p = String.lowercase(String.sub s 0 lp))


(* FIXME: using Knutt-Moris-Pratt algo, one should be able to parse
   post-data as it comes (from packets or stdin).  Requires a special
   Buffer module with deletion.  *)
(* FIXME: enforce max sizes *)
let parse_post_data data request =
  let content_type = metavar_string request "CONTENT_TYPE" in
  (* media-type = type "/" subtype (";" parameter)* *)
  if is_prefix_ci content_type "application/x-www-form-urlencoded" then
    parse_query data request.params
  else if is_prefix_ci "multipart/form-data" content_type then begin
    request.is_multipart <- true;
    let boundary = get_boundary content_type in
    try
      parse_multipart boundary data request
    with
      Not_found -> () (* Ignore parts without final delim,... *)
  end
  else raise(Unsupported_media_type content_type)



(* Date manipulation
 ***********************************************************************)

let string_of_weekday =
  let wd = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"; "Sun" |] in
  fun i -> Array.unsafe_get wd i

let string_of_month =
  let month = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug";
		 "Sep"; "Oct"; "Nov"; "Dec" |] in
  fun i -> Array.unsafe_get month i


(* Error pages
 ***********************************************************************)

let error_html errn msg email =
  let t = Unix.localtime(Unix.time()) in
  Printf.sprintf "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\" lang=\"en\">
<html><body>
<h1>%s</h1>
<p>The server encountered an internal error or misconfiguration
and was unable to complete your request.
</p>
<p>Please contact the server administrator,
<a href=\"mailto:%s?subject=%s\">%s</a>,
and inform him of the time the error
occurred (%s %s %i %02i:%02i:%02i %i),
the error message \"%s\",
and anything you might have done that resulted in this error.
</p>
<br/>
<hr/>
<i><a href=\"http://www.sf.net/ocaml-cgi\">CamlGI</a>
-- a powerful <a href=\"http://caml.inria.fr/\">OCaml</a> library
to easily create (Fast)CGI applications!
</i>
</body></html>"
    (std_error_msg errn) email msg email
    (string_of_weekday t.Unix.tm_wday) (string_of_month t.Unix.tm_mon)
    t.Unix.tm_mday  t.Unix.tm_hour  t.Unix.tm_min t.Unix.tm_sec
    (t.Unix.tm_year + 1900)
    msg
