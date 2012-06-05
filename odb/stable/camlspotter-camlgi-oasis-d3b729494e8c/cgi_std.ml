(* File: cgi_std.ml

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
(*	$Id: cgi_std.ml,v 1.6 2005/06/12 21:38:12 chris_77 Exp $	*)

open Cgi_types
open Cgi_common


(* [log msg] logs the message [msg] in the server log. *)
let log msg =
  let t = Unix.localtime(Unix.time()) in
  Printf.eprintf "[%s %s %i %02i:%02i:%02i %i] %s: %s\n"
    (string_of_weekday t.Unix.tm_wday) (string_of_month t.Unix.tm_mon)
    t.Unix.tm_mday  t.Unix.tm_hour  t.Unix.tm_min t.Unix.tm_sec
    (t.Unix.tm_year + 1900)
    Sys.executable_name
    msg


(* Send headers corresponding with Status = [errn] and message
   [err_msg]. *)
let close_request_error r errn err_msg =
  if r.header_not_emitted then
    Printf.printf "Status: %i %s\r\nContent-Type: text/html\r\n\r\n"
      errn err_msg;
  let email =
    try Hashtbl.find r.metavars "SERVER_ADMIN"
    with Not_found -> "webmaster@" ^ (Unix.gethostname()) in
  print_string(error_html errn err_msg email);
  exit 1


(* [handle_request_error f request] apply [f] to the request
   [request], sending an error status for uncaught exceptions and
   logging them. *)
let handle_request_error f request =
  try
    f request;
    exit 0
  with
  | Exit ->
      if request.header_not_emitted then
	print_string "Status: 204 No Response\r\n\r\n";
      exit 0
  | Abort ->
      log "Exception \"Abort\" not caught";
      close_request_error request cHTTP_PARTIAL_CONTENT "Script aborted"
  | HttpError e ->
      log("Exception \"HttpError(" ^ string_of_int e ^ ")\".");
      close_request_error request e ("HTTP Error " ^ string_of_int e)
  | e ->
      log("Uncaught exception " ^ Printexc.to_string e);
      close_request_error request cHTTP_INTERNAL_SERVER_ERROR
	"Internal Server Error"



(* The initialization must only be triggered when the function is
   called, since, before that, one is not sure we have to treat the
   data as CGI. *)
let handle_request fork f conn =
  (* Building a hash of the meta-variables *)
  let metavars = Hashtbl.create 40 in
  Array.iter (fun s ->
		try
		  let ieq = String.index s '=' in
		  let i1 = ieq + 1 in
		  let name = String.sub s 0 ieq
		  and value = String.sub s i1 (String.length s - i1) in
		  Hashtbl.add metavars name value
		with Not_found -> ()
	     ) (Unix.environment());
  (* GATEWAY_INTERFACE -- version *)
  let gateway =
    try
      let gw = Hashtbl.find metavars "GATEWAY_INTERFACE" in
      let i1 = String.index gw '/' + 1 in
      let i2 = String.index_from gw i1 '.' in
      let i3 = i2 + 1 in
      (* int_of_string can cope with leading zeroes *)
      let major = int_of_string(String.sub gw i1 (i2 - i1))
      and minor = int_of_string(String.sub gw i3 (String.length gw - i3)) in
      CGI(major, minor)
    with Not_found | Failure _ -> CGI(1,1) in
  (* Preparing [request] to pass to [f] *)
  let request = {
    role = Responder;
    gateway = gateway;
    metavars = metavars;
    params = Hashtbl.create 10;
    is_multipart = false;
    uploads = Hashtbl.create 1;
    print_string = Pervasives.print_string;
    prerr_string = Pervasives.prerr_string;
    header_not_emitted = true;
    (* FCGI ouput -- not used here *)
    access = Mutex.create();
    stdout = Buffer.create 0;
    stderr = Buffer.create 0;
    (* FCGI managment fields -- unused here *)
    id = 1;
    keep_conn = false;
    status = Handler_launched;
    buf = Buffer.create 0;
    abort = false;
  } in
  let rmethod = metavar_string request "REQUEST_METHOD" in
  match String.uppercase rmethod with
  | "GET" | "HEAD" ->
      let qs = metavar_string request "QUERY_STRING" in
      parse_query qs request.params;
      handle_request_error f request
  | "POST" ->
      (* FIXME: do not really want to read the whole input into mem
	 but that will do as long as the [upload] struct is as it is.
      *)
      (* FIXME: Obey max requested sizes *)
      let len = metavar_int request "CONTENT_LENGTH" ~default:0 in
      if len > Sys.max_string_length then
	close_request_error request 413 "Request Entity Too Large";
      let data = String.create len in
      begin
	try really_input stdin data 0 len
	with End_of_file ->
	  close_request_error request cHTTP_BAD_REQUEST
	    "Not enough data on input"
      end;
      begin
	try parse_post_data data request
	with Unsupported_media_type t ->
	  close_request_error request 415 ("Unsupported Media Type: " ^ t)
      end;
      handle_request_error f request
  | _ ->
      (* FIXME: The following methods are currently unsupported "PUT"
	 | "DELETE" | "OPTIONS" | "TRACE" *)
      (* Unknown method *)
      close_request_error request 405 "Method Not Allowed"
