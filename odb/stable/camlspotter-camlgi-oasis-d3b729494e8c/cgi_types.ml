(* File: cgi_types.ml

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
(* 	$Id: cgi_types.ml,v 1.7 2005/01/25 19:42:14 chris_77 Exp $	 *)


type upload_data = {
  upload_value: string;
  upload_filename: string;
  upload_content_type: string;
}

type role =
  | Responder
  | Authorizer
  | Filter

type gateway =
  | CGI of int * int
  | FCGI of int

type status =
  | Get_params
  | Get_stdin
  | Get_data
  | Handler_launched

(* A request gathered from the Web server *)
type request = {
  role : role;
  gateway : gateway;
  metavars : (string, string) Hashtbl.t; (* CGI metavariables, FCGI
					    calls them PARAMS *)
  params : (string, string) Hashtbl.t; (* parameters to the script,
					  via GET or POST *)
  mutable is_multipart : bool;
  uploads : (string, upload_data) Hashtbl.t;
  print_string : string -> unit;
  prerr_string : string -> unit;
  mutable header_not_emitted : bool;
  (* FCGI output *)
  access : Mutex.t;
  stdout : Buffer.t;
  stderr : Buffer.t;
  (* Outputs need to be buffered for the process handling multiplexed
     requests to send them on the socket in a orderly manner.  We
     protect these buffers in between these two processes; at least
     when there are multiplexed requests. *)

  (* FCGI management fields *)
  id : int;
  keep_conn : bool;
  mutable status : status;
  buf : Buffer.t; (* temporary buffer for STDIN, then contains DATA
		     (if any). *)
  mutable abort : bool;
}

type connection = {
  fd : Unix.file_descr;
  max_conns : int;
  max_reqs : int;
  handle_requests : ((request -> unit) -> request -> unit) ->
		  (request -> unit) -> connection ->  unit;
  (* [handle_requests fork f req] function handling the protocol.  It
     is here because it is known after [establish_server] is issued
     (one may imagine that [stdin] is connected for external servers)
     but remains the same for each connection (thus no test each
     time).  *)
  requests : (int, request) Hashtbl.t;
  (* Structure holding the various requests.  The thread handling the
     request will be direcly fed with [request] and will not need to
     use [requests] -- thus only the process managing the connection
     will change [requests] and it needs not to be protected. *)
}



(* CGI/1.1 does not make a difference between a non-existent
   metavariable and a metavariable with value "". *)
let metavar_string request name =
  try Hashtbl.find request.metavars name with _ -> ""

let metavar_int request name ~default =
  try int_of_string(Hashtbl.find request.metavars name)
  with _ -> default


exception Abort
  (* Exception raised if an abort request is sent. *)

exception Ignore_record
  (* Raised if the data sent by the web sever does not conform the
     spec or because we do not care what such request is (e.g. for a
     "GET" we do not need stdin). *)

exception HttpError of int

(* Names for HTTP errors
 ***********************************************************************)

(* Apache *)
let cHTTP_CONTINUE =                      100
let cHTTP_SWITCHING_PROTOCOLS =           101
let cHTTP_PROCESSING =                    102
let cHTTP_OK =                            200
let cHTTP_CREATED =                       201
let cHTTP_ACCEPTED =                      202
let cHTTP_NON_AUTHORITATIVE =             203
let cHTTP_NO_CONTENT =                    204
let cHTTP_RESET_CONTENT =                 205
let cHTTP_PARTIAL_CONTENT =               206
let cHTTP_MULTI_STATUS =                  207
let cHTTP_MULTIPLE_CHOICES =              300
let cHTTP_MOVED_PERMANENTLY =             301
let cHTTP_MOVED_TEMPORARILY =             302
let cHTTP_SEE_OTHER =                     303
let cHTTP_NOT_MODIFIED =                  304
let cHTTP_USE_PROXY =                     305
let cHTTP_TEMPORARY_REDIRECT =            307
let cHTTP_BAD_REQUEST =                   400
let cHTTP_UNAUTHORIZED =                  401
let cHTTP_PAYMENT_REQUIRED =              402
let cHTTP_FORBIDDEN =                     403
let cHTTP_NOT_FOUND =                     404
let cHTTP_METHOD_NOT_ALLOWED =            405
let cHTTP_NOT_ACCEPTABLE =                406
let cHTTP_PROXY_AUTHENTICATION_REQUIRED = 407
let cHTTP_REQUEST_TIME_OUT =              408
let cHTTP_CONFLICT =                      409
let cHTTP_GONE =                          410
let cHTTP_LENGTH_REQUIRED =               411
let cHTTP_PRECONDITION_FAILED =           412
let cHTTP_REQUEST_ENTITY_TOO_LARGE =      413
let cHTTP_REQUEST_URI_TOO_LARGE =         414
let cHTTP_UNSUPPORTED_MEDIA_TYPE =        415
let cHTTP_RANGE_NOT_SATISFIABLE =         416
let cHTTP_EXPECTATION_FAILED =            417
let cHTTP_UNPROCESSABLE_ENTITY =          422
let cHTTP_LOCKED =                        423
let cHTTP_FAILED_DEPENDENCY =             424
let cHTTP_INTERNAL_SERVER_ERROR =         500
let cHTTP_NOT_IMPLEMENTED =               501
let cHTTP_BAD_GATEWAY =                   502
let cHTTP_SERVICE_UNAVAILABLE =           503
let cHTTP_GATEWAY_TIME_OUT =              504
let cHTTP_VERSION_NOT_SUPPORTED =         505
let cHTTP_VARIANT_ALSO_VARIES =           506
let cHTTP_INSUFFICIENT_STORAGE =          507
let cHTTP_NOT_EXTENDED =                  510

(* Common aliases *)
let cDOCUMENT_FOLLOWS =    cHTTP_OK
let cPARTIAL_CONTENT =     cHTTP_PARTIAL_CONTENT
let cMULTIPLE_CHOICES =    cHTTP_MULTIPLE_CHOICES
let cMOVED =               cHTTP_MOVED_PERMANENTLY
let cREDIRECT =            cHTTP_MOVED_TEMPORARILY
let cUSE_LOCAL_COPY =      cHTTP_NOT_MODIFIED
let cBAD_REQUEST =         cHTTP_BAD_REQUEST
let cAUTH_REQUIRED =       cHTTP_UNAUTHORIZED
let cFORBIDDEN =           cHTTP_FORBIDDEN
let cNOT_FOUND =           cHTTP_NOT_FOUND
let cMETHOD_NOT_ALLOWED =  cHTTP_METHOD_NOT_ALLOWED
let cNOT_ACCEPTABLE =      cHTTP_NOT_ACCEPTABLE
let cLENGTH_REQUIRED =     cHTTP_LENGTH_REQUIRED
let cPRECONDITION_FAILED = cHTTP_PRECONDITION_FAILED
let cSERVER_ERROR =        cHTTP_INTERNAL_SERVER_ERROR
let cNOT_IMPLEMENTED =     cHTTP_NOT_IMPLEMENTED
let cBAD_GATEWAY =         cHTTP_BAD_GATEWAY
let cVARIANT_ALSO_VARIES = cHTTP_VARIANT_ALSO_VARIES

let std_error_msg = function
  | 100 -> "100 Continue"
  | 101 -> "101 Switching Protocols"
  | 102 -> "102 Processing"
  | 200 -> "200 Ok"
  | 201 -> "201 Created"
  | 202 -> "202 Accepted"
  | 203 -> "203 Non Authoritative"
  | 204 -> "204 No Content"
  | 205 -> "205 Reset Content"
  | 206 -> "206 Partial Content"
  | 207 -> "207 Multi Status"
  | 300 -> "300 Multiple Choices"
  | 301 -> "301 Moved Permanently"
  | 302 -> "302 Moved Temporarily"
  | 303 -> "303 See Other"
  | 304 -> "304 Not Modified"
  | 305 -> "305 Use Proxy"
  | 307 -> "307 Temporary Redirect"
  | 400 -> "400 Bad Request"
  | 401 -> "401 Unauthorized"
  | 402 -> "402 Payment Required"
  | 403 -> "403 Forbidden"
  | 404 -> "404 Not Found"
  | 405 -> "405 Method Not Allowed"
  | 406 -> "406 Not Acceptable"
  | 407 -> "407 Proxy Authentication Required"
  | 408 -> "408 Request Time Out"
  | 409 -> "409 Conflict"
  | 410 -> "410 Gone"
  | 411 -> "411 Length Required"
  | 412 -> "412 Precondition Failed"
  | 413 -> "413 Request Entity Too Large"
  | 414 -> "414 Request Uri Too Large"
  | 415 -> "415 Unsupported Media Type"
  | 416 -> "416 Range Not Satisfiable"
  | 417 -> "417 Expectation Failed"
  | 422 -> "422 Unprocessable Entity"
  | 423 -> "423 Locked"
  | 424 -> "424 Failed Dependency"
  | 500 -> "500 Internal Server Error"
  | 501 -> "501 Not Implemented"
  | 502 -> "502 Bad Gateway"
  | 503 -> "503 Service Unavailable"
  | 504 -> "504 Gateway Time Out"
  | 505 -> "505 Version Not Supported"
  | 506 -> "506 Variant Also Varies"
  | 507 -> "507 Insufficient Storage"
  | 510 -> "510 Not Extended"
  | _ -> "Internal Server Error"
