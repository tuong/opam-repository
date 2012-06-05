(* File: cookie.ml

   Objective Caml Library for writing (F)CGI programs.

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
(* 	$Id: cookie.ml,v 1.2 2005/01/25 19:40:37 chris_77 Exp $	 *)

open Cgi_common


class cookie ~name ~value ~max_age ~domain ~path ~secure =
object (self)
  val mutable name = name
  val mutable value = value
    (*     val mutable comment = comment *)
  val mutable domain = domain
  val mutable max_age = max_age
  val mutable path = path
  val mutable secure = secure

  method name = name
  method value = value
  method domain = domain
  method max_age = max_age
  method path = path
  method secure = secure

  method set_name v = name <- v
  method set_value v = value <- v
  method set_domain v = domain <- v
  method set_max_age v = max_age <- v
  method set_path v = path <- v
  method set_secure v = secure <- v

  method to_string =
    let buf = Buffer.create 128 in
    if String.length name > 0 && String.unsafe_get name 0 = '$' then
      (* TRICK: names cannot start with '$', so if it does  add '+'
	 in front to protect it. '+' will be decoded as space, then
	 stripped. *)
      Buffer.add_char buf '+';
    Buffer.add_string buf (encode_cookie name);
    Buffer.add_char buf '=';
    Buffer.add_string buf (encode_cookie value);
    Buffer.add_string buf "; Version=1";
    (* We do not encode the domain and path because they will be
       interpreted by the browser to determine whether the cookie
       must be sent back. *)
    if domain <> "" then begin
      Buffer.add_string buf "; domain=";
      Buffer.add_string buf domain;
    end;
    if path <> "" then begin
      Buffer.add_string buf "; path=";
      Buffer.add_string buf path;
    end;
    if secure then Buffer.add_string buf "; secure";
    begin match max_age with
    | None -> ()
    | Some m ->
	Buffer.add_string buf "; Max-Age=";
	Buffer.add_string buf (if m >= 0 then (string_of_int m) else "0");
	(* For compatibility with old browsers: *)
	Buffer.add_string buf "; expires=";
	Buffer.add_string buf (if m > 0 then (Expires.make m)
			       else "Thu, 1 Jan 1970 00:00:00 GMT");
    end;
    Buffer.contents buf
end


let cookie ?max_age ?(domain="") ?(path="") ?(secure=false) name value =
  new cookie ~name ~value ~domain ~max_age ~path ~secure


let parse =
  let make_cookie s =
    let name, value =
      try
	let i = String.index s '=' in
	(* FIXME: Must support quoted strings? *)
	(* FIXME: $Version, $Path, $Domain *)
	(* Here it is important that we strip heading and trailing spaces *)
	(decode_range s 0 i,
	 decode_range s (succ i) (String.length s))
      with
	Not_found -> (decode_range s 0 (String.length s), "") in
    cookie name value in
  fun header ->
    if header = "" then []
    else List.map make_cookie (rev_split ';' header)
