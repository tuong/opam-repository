(* File: cgi.ml

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
(* 	$Id: cgi.ml,v 1.18 2005/06/12 21:35:46 chris_77 Exp $	 *)


open Cgi_common
include Cgi_types
open Printf


module Request =
struct
  type t = Cgi_types.request

  type role = Cgi_types.role =
    | Responder
    | Authorizer
    | Filter

  type gateway = Cgi_types.gateway =
    | CGI of int * int
    | FCGI of int

  let gateway r = r.gateway
  let role r = r.role

  let metavar r name =
    try Hashtbl.find r.metavars name with Not_found -> ""

  let path_info r = metavar r "PATH_INFO"
  let protocol r = String.uppercase(metavar r "SERVER_PROTOCOL")
  let remote_addr r = metavar r "REMOTE_ADDR"
  let server_name r = metavar r "SERVER_NAME"
  let server_port r =
    try int_of_string(metavar r "SERVER_PORT") with _ -> 80
  let server_software r = metavar r "SERVER_SOFTWARE"

  let accept r = metavar r "HTTP_ACCEPT"
  let accept_charset r = metavar r "HTTP_ACCEPT_CHARSET"
  let accept_encoding r = metavar r "HTTP_ACCEPT_ENCODING"
  let auth r = metavar r "AUTH_TYPE"
  let user r = metavar r "REMOTE_USER"
  let user_agent r = metavar r "HTTP_USER_AGENT"

  let print_string r s = r.print_string s
  let prerr_string r s = r.prerr_string s
end


(* Generate a suitable random number (32 hex digits) for use in random
   cookies, session IDs, etc.  These numbers are supposed to be very
   hard to predict.  *)
let random_sessionid =
  if Sys.file_exists "/dev/urandom" then begin
    fun () ->
      let s = String.create 32 (* local => thread safe *) in
      let chan = open_in_bin "/dev/urandom" in
      for i = 0 to 15 do
	let b = input_byte chan in
	let i2 = 2 * i in
	s.[i2] <- char_of_hex(b lsr 4);
	s.[i2 + 1] <- char_of_hex(b land 0x0F)
      done;
      close_in chan;
      s
  end
  else begin
    Random.self_init();
    fun () ->
      let s = String.create 32 in
      for i = 0 to 7 do
	let b = Random.int 0x10000
	and i4 = 4 * i in
	s.[i4] <- char_of_hex(b land 0xF);
	let b = b lsr 4 in
	s.[i4 + 1] <- char_of_hex(b land 0xF);
	let b = b lsr 4 in
	s.[i4 + 2] <- char_of_hex(b land 0xF);
	s.[i4 + 3] <- char_of_hex(b lsr 4)
      done;
      s
  end



(* cgi object
 **********************************************************************
 * The fact that it is a CGI or a fast CGI is hidden by the object.
 *)

class type template =
object
  method output : (string -> unit) -> unit
end

let cookie_header r ~(cookie:Cookie.cookie option)
    ~(cookies:Cookie.cookie list option) cookie_cache =
  (match cookie with
   | None -> ()
   | Some c -> r.print_string("Set-Cookie: " ^ c#to_string ^ "\r\n"));
  (match cookies with
   | None -> ()
   | Some cs ->
       List.iter(fun c -> r.print_string("Set-Cookie: "
					 ^ c#to_string ^ "\r\n")) cs);
  if not cookie_cache then begin
    r.print_string "Cache-control: no-cache=\"set-cookie\"\r\n";
    (* For HTTP/1.0 proxies along the way.  Cache-control directives
       override this for HTTP/1.1.  *)
    r.print_string "Expires: Thu, 1 Jan 1970 00:00:00 GMT\r\n";
    r.print_string "Pragma: no-cache\r\n"
  end


class cgi r =
  let cheader =
    try Hashtbl.find r.metavars "HTTP_COOKIE" with Not_found -> "" in
  let cookies = Cookie.parse cheader in
  let log =
    match r.gateway with
    | CGI _ -> Cgi_std.log
    | FCGI _ -> r.prerr_string (* Date and executable name already
				  provided by FCGI handler *)
  in
object(self)

  method header ?(content_type="text/html")
    ?cookie ?cookies ?(cookie_cache=false) () =
    if r.abort then raise Abort;
    if r.header_not_emitted then begin
      cookie_header r ?cookie ?cookies cookie_cache;
      r.print_string(sprintf "Content-type: %s\r\nStatus: 200 OK\r\n\r\n"
		       content_type);
      r.header_not_emitted <- false;
    end

  method template : 'a. ?content_type:string -> ?cookie:Cookie.cookie ->
    ?cookies:Cookie.cookie list -> ?cookie_cache:bool ->
    (#template as 'a) -> unit =
    fun ?content_type ?cookie ?cookies ?(cookie_cache=false) template ->
      self#header ?content_type ?cookie ?cookies ();
      template#output (fun s -> r.print_string s)

  method exit : 'a. unit -> 'a = (fun () -> raise Exit)

  method redirect : 'a. ?cookie:Cookie.cookie ->
    ?cookies:Cookie.cookie list -> ?cookie_cache:bool -> string -> 'a
    = fun ?cookie ?cookies ?(cookie_cache=false) url ->
      cookie_header r ?cookie ?cookies cookie_cache;
      r.print_string("Location: " ^ url ^ "\r\n\r\n"); (* FIXME *)
      raise(HttpError cHTTP_MOVED_TEMPORARILY)

  method url () =
    if r.abort then raise Abort;
    try Hashtbl.find r.metavars "SCRIPT_NAME"
    with Not_found -> ""

  method param name =
    if r.abort then raise Abort;
    Hashtbl.find r.params name

  method param_all name =
    if r.abort then raise Abort;
    Hashtbl.find_all r.params name

  method param_exists name =
    if r.abort then raise Abort;
    try  ignore (self#param name); true
    with Not_found -> false

  method param_true name =
    try  let str = self#param name in str <> "" && str <> "0"
    with Not_found -> false

  method params =
    if r.abort then raise Abort;
    Hashtbl.fold (fun k v l -> (k,v) :: l) r.params []

  method is_multipart =
    if r.abort then raise Abort;
    (Hashtbl.length r.uploads > 0)

  method upload name =
    if r.abort then raise Abort;
    Hashtbl.find r.uploads name

  method upload_all name =
    if r.abort then raise Abort;
    Hashtbl.find_all r.uploads name

  method cookie name =
    if r.abort then raise Abort;
    List.find (fun cookie -> cookie#name = name) cookies

  method cookies =
    if r.abort then raise Abort;
    cookies

  method log = log

  method request = r
end


module Cgi_args =
struct
  (* We will not use [parse_query_range] because the user may not
     expect the query string to be overwritten.  In order not to copy
     the entire query string (which may be big), we will split it
     first. *)
  let parse qs =
    let key_val = rev_split '&' qs in
    let split s =
      try
	let i = String.index s '=' in
	(decode_range s 0 i,  decode_range s (succ i) (String.length s))
      with
	Not_found -> (decode_range s 0 (String.length s), "")   in
    List.rev_map split key_val


  let make bindings =
    let encode (key, value) = encode key ^ "=" ^ encode value in
    String.concat "&" (List.map encode bindings)
end



(* Registering scripts
 ***********************************************************************)

type cgi_type =
  | CGI_std	(* Standard CGI *)
  | CGI_socket	(* FastCGI -- Unix sockets *)
  | CGI_pipe	(* FastCGI -- Win named pipes *)

(* Test whether the script is used as fastcgi. *)
let cgi_type() =
  match Sys.os_type with
  | "Unix" | "Cygwin" ->
      begin
	try let _ = Unix.getpeername Cgi_fast.fcgi_listensock in CGI_std
	with
	| Unix.Unix_error(Unix.ENOTCONN, _, _) -> CGI_socket
	| Unix.Unix_error(_, _, _) -> CGI_std
      end
  | "Win32" ->
      begin
	try let _ = Unix.getenv "REQUEST_METHOD" in CGI_std
	with Not_found -> CGI_pipe
      end
  | os -> failwith("Unsupported platform: " ^ os)


let no_fork f req = f req

(* FIXME: post_max unused *)
let establish_server ?(max_conns=1) ?(max_reqs=1) ?sockaddr
    ?(post_max=Sys.max_string_length) (f : connection -> unit) =
  match sockaddr with
  | None ->
      begin match cgi_type() with
      | CGI_std ->
	  (* Normal CGI -- prepare a single request *)
	  f { fd = Unix.stdin;
	      max_conns = 1;
	      max_reqs = 1;
	      handle_requests = Cgi_std.handle_request;
	      requests = Hashtbl.create 0;
	    }
      | CGI_socket ->
	  Cgi_fast.establish_server_socket ~max_conns ~max_reqs
	    Cgi_fast.fcgi_listensock f
      | CGI_pipe ->
	  Cgi_fast.establish_server_pipe ~max_conns:1 ~max_reqs
	    Cgi_fast.fcgi_listensock f
      end

  | Some sockaddr ->
      (* FastCGI on a distant machine, listen on the given socket. *)
      let sock =
	Unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
      Unix.setsockopt sock Unix.SO_REUSEADDR true;
      Unix.bind sock sockaddr;
      Cgi_fast.establish_server_socket ~max_conns ~max_reqs sock f


let handle_requests ?(fork=no_fork) f conn =
  conn.handle_requests fork f conn

let register_script ?sockaddr f =
  establish_server ~max_conns:1 ~max_reqs:1 ?sockaddr
    (fun conn -> conn.handle_requests no_fork f conn)
