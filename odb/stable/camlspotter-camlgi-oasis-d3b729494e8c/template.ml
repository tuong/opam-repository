(* File: template.ml

   Copyright (C) 2004

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
(* 	$Id: template.ml,v 1.4 2006/01/08 22:22:02 chris_77 Exp $	 *)


(* Escaping
 ***********************************************************************)
(* FIXME: The functions below are limited -- they do not escape
   everything well. *)

type escape =
    EscapeNone | EscapeUrl | EscapeHtml | EscapeHtmlTag | EscapeHtmlTextarea

let amp_re = Str.regexp_string "&"
let lt_re = Str.regexp_string "<"
let gt_re = Str.regexp_string ">"
let quot_re = Str.regexp_string "\""
let eol_re = Str.regexp "\r?\n"

let escape_html_textarea str =
  let str = Str.global_replace amp_re  "&amp;"  str in
  let str = Str.global_replace lt_re   "&lt;"   str in
  let str = Str.global_replace gt_re   "&gt;"   str in
  let str = Str.global_replace quot_re "&quot;" str in
  str

let escape_html str =
  let str = escape_html_textarea str in
  let str = Str.global_replace eol_re "<br/>" str in
  str

let escape_html_tag str =
  let str = escape_html_textarea str in
  let str = Str.global_replace eol_re "&#13;&#10;" str in
  str

let escape = function
  | EscapeNone -> (fun str -> str)
  | EscapeUrl -> Cgi_common.encode
  | EscapeHtml -> escape_html
  | EscapeHtmlTag -> escape_html_tag
  | EscapeHtmlTextarea -> escape_html_textarea


(* Inputting files
 ***********************************************************************)

let input_whole_chan chan =
  let buf = Buffer.create 4096 in
  try
    while true do
      Buffer.add_string buf (input_line chan);
      Buffer.add_char buf '\n';
    done;
    assert false
  with
    End_of_file -> Buffer.contents buf

let input_whole_file filename =
  let fh = open_in filename in
  let file_contents = input_whole_chan fh in
  close_in fh;
  file_contents


(* Compile templates
 ***********************************************************************)

type compiled = node list
and node =
  | Plain of string 			(* Just some text. *)
  | Var of escape * string		(* ::tag:: *)
  | If of string * compiled * compiled  (* ::if(..):: .. ::else:: .. ::end:: *)
  | Table of string * compiled		(* ::table(..):: .. ::end:: *)
  | Call of escape * string * string list

(* [prepend_rev l1 l2] = [List.rev l1 @ l2] *)
let rec prepend_rev l1 l2 =
  match l1, l2 with
  | [], _ -> l2
  | a :: tl, _ -> prepend_rev tl (a :: l2)

(* If name has the form "<name>_html" (or one of the similar cases),
   return bare name and the form of the escaping that needs to be applied. *)
let get_var =
  let string_ends_with s sfx =
    let ls = String.length s
    and lsfx = String.length sfx in
    ls >= lsfx && String.sub s (ls - lsfx) lsfx = sfx in
  let suppress s sfx = String.sub s 0 (String.length s - String.length sfx) in
  fun name ->
    if string_ends_with name "_url" then
      Var(EscapeUrl, suppress name "_url")
    else if string_ends_with name "_html" then
      Var(EscapeHtml, suppress name "_html")
    else if string_ends_with name "_html_tag" then
      Var(EscapeHtmlTag, suppress name "_html_tag")
    else if string_ends_with name "_html_textarea" then
      Var(EscapeHtmlTextarea, suppress name "_html_textarea")
    else Var(EscapeNone, name)


let delim_re = Str.regexp_string "::"
let ident = "[ \t]*\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)[ \t]*"
let var_re = Str.regexp("::" ^ ident ^"::")
let if_re = Str.regexp("::if(" ^ ident ^ ")::")
let else_re = Str.regexp_string "::else::"
let end_re = Str.regexp_string "::end::"
let table_re = Str.regexp("::table(" ^ ident ^ ")::")
let include_re =
  (* We do not allow ' ' in filenames because it is too easy to get
     mistaken. *)
  Str.regexp("::include([ \t\r\n]*\\([^(): \t\r\n]+\\)[ \t\r\n]*)::")
let call_re =
  Str.regexp("::call(" ^ ident
	     ^ "\\(\\([ \t]*,[ \t]*[a-zA-Z_]+[a-zA-Z0-9_]*\\)*\\)[ \t]*)"
	     ^ "\\([_a-z]*\\)::")
let comma_re = Str.regexp "[ \t]*,[ \t]*"

(* The compiled templates are not mutable.  We keep a list of
   templates associated to filenames to reuse them if necessary
   (e.g. when including headers and footers). *)
let compiled_templates = Hashtbl.create 5

(* Type of delimiters that ends the current level of recursion. *)
type closing =
  | Else (* ::else:: *)
  | End  (* ::end:: *)
  | Eos  (* End of string *)

let rec compile_template ~filename source =
  match filename with
  | None ->
      let ct, _, _ =
	compile (Sys.getcwd()) source (String.length source) 0 [] in
      ct
  | Some f ->
      try Hashtbl.find compiled_templates f
      with Not_found ->
	let ct, _, _ =
	  compile (Filename.dirname f) source (String.length source) 0 [] in
	Hashtbl.add compiled_templates f ct;
	ct

(* [compile basename source len i0 ct] returns [(ct', closing, i)]
   where [ct'] is the compiled template, [closing] is the "token"
   closing that part and [i] is the position right after that token.
   [ct] is the compiled template SO FAR in REVERSE order. *)
and compile basename source len i0 ct =
  try
    if i0 >= len then raise Not_found;
    let i = Str.search_forward delim_re source i0 in
    let ct =
      if i > i0 then Plain(String.sub source i0 (i - i0)) :: ct else ct in
    (* Check if any of the recognized constructs follows *)
    if Str.string_match end_re source i then
      (List.rev ct, End, Str.match_end())
    else if Str.string_match else_re source i then
      (List.rev ct, Else, Str.match_end())
    else if Str.string_match var_re source i then
      let ct = get_var(Str.matched_group 1 source) :: ct in
      compile basename source len (Str.match_end()) ct

    else if Str.string_match if_re source i then
      let cond = Str.matched_group 1 source in
      let i_end = Str.match_end() in
      let (ct_then, cl, i) = compile basename source len i_end [] in
      match cl with
      | Else ->
	  let (ct_else, cl, i) = compile basename source len i [] in
	  begin match cl with
	  | End ->
	      compile basename source len i (If(cond, ct_then, ct_else) :: ct)
	  | _ -> failwith("Missing ::end:: for ::if(" ^ cond
			  ^ "):: ... ::else::")
	  end
      | End ->
	  compile basename source len i (If(cond, ct_then, []) :: ct)
      | Eos -> failwith("Missing ::end:: for ::if(" ^ cond ^ ")::")

    else if Str.string_match table_re source i then
      let table = Str.matched_group 1 source in
      let i_end = Str.match_end() in
      let (ct_rows, cl, i) = compile basename source len i_end [] in
      match cl with
      | End ->
	  compile basename source len i (Table(table, ct_rows) :: ct)
      | _ -> failwith("Missing ::end:: for ::table(" ^ table ^ ")::")

    else if Str.string_match call_re source i then
      let fname = Str.matched_group 1 source in
      let args = Str.matched_group 2 source in
      let esc =
	match Str.matched_group 4 source with
	| "_url" -> EscapeUrl
	| "_html" -> EscapeHtml
	| "_html_tag" -> EscapeHtmlTag
	| "_html_textarea" -> EscapeHtmlTextarea
	| _ -> EscapeNone in
      let i_end = Str.match_end() in
      let ct = Call(esc, fname, Str.split comma_re args) :: ct in
      compile basename source len i_end ct

    else if Str.string_match include_re source i then
      let filename = Str.matched_group 1 source in
      let i_end = Str.match_end() in
      let filename =
	if Filename.is_relative filename
	then Filename.concat basename filename
	else filename in
      let source = input_whole_file filename in
      let ct_incl = compile_template ~filename:(Some filename) source in
      compile basename source len i_end (prepend_rev ct_incl ct)

    else
      (* Skip "::" and look for the next one *)
      compile basename source len (i + 2) (Plain "::" :: ct)

  with Not_found ->
    let ct =
      if len > i0 then Plain(String.sub source i0 (len - i0)) :: ct else ct in
    (List.rev ct, Eos, len)


(* Set variables and output the template
 ***********************************************************************)

type var =
  | VarString of string				(* ::tag:: *)
  | VarTable of table_row list			(* ::table(tag):: *)
  | VarConditional of bool			(* ::if(tag):: *)
  | VarCallback of (string list -> string)	(* ::call(f, x1,...):: *)
and table_row = (string * var) list


let find bindings k =
  try  Hashtbl.find bindings k
  with Not_found ->
    failwith("CamlGI.Template: tag/table ::" ^ k
	     ^ ":: was not assigned any value.")

class template ?filename source =
  let bindings = Hashtbl.create 5 in
  let ct0 = compile_template ?filename source in
object(self)
  method set name value = Hashtbl.add bindings name (VarString value)

  method table name tbl = Hashtbl.add bindings name (VarTable tbl)

  method conditional name cond =
    Hashtbl.add bindings name (VarConditional cond)

  method callback name f = Hashtbl.add bindings name (VarCallback f)

  method to_string =
    let buffer = Buffer.create 4096 in
    self#output (Buffer.add_string buffer);
    Buffer.contents buffer

  method to_channel chan =
    self#output (output_string chan)

  method source =
    source

  method output =
    let resolve_variable name =
      match find bindings name with
      | VarString str -> str
      | _ -> failwith ("CamlGI.Template: ::" ^ name
		       ^ ":: should be a simple string tag.")
    and eval_condition name =
      match find bindings name with
      | VarConditional b -> b
      | _ -> failwith ("CamlGI.Template: ::if(" ^ name
		       ^ "):: should be a conditional tag.")
    and resolve_table name =
      match find bindings name with
      | VarTable tbl -> tbl
      | _ -> failwith ("CamlGI.Template: ::table(" ^ name
		       ^ "):: should be a table tag.")
    and resolve_callback name =
      match find bindings name with
      | VarCallback f -> f
      | _ -> failwith ("CamlGI.Template: ::call(" ^ name
		       ^ "[,...]):: should be a callback function.") in
    let rec substitute (out:string -> unit) ct =
      let out_node = function
	| Plain text -> out text
	| Var(esc, name) ->
	    out(escape esc (resolve_variable name))
	| If(cond, then_clause, else_clause) ->
	    substitute out (if eval_condition cond
			    then then_clause else else_clause)
	| Table(name, body) ->
	    (* For each table [row], add the corresponding bindings,
	        process the table body and then remove them. *)
	    let process_row row =
	      List.iter (fun (k, v) -> Hashtbl.add bindings k v) row;
	      substitute out body;
	      List.iter (fun (k, v) -> Hashtbl.remove bindings k) row;
	    in
	    List.iter process_row (resolve_table name);
	| Call(esc, fname, args) ->
	    out(escape esc (resolve_callback fname args)) in
      List.iter out_node ct  in
    fun out -> substitute out ct0
end


let template_from_string ?filename source =
  new template ?filename source

let template_from_channel ?filename chan =
  template_from_string ?filename (input_whole_chan chan)

let template filename =
  template_from_string ~filename (input_whole_file filename)

