(* File: cgi_fast.ml

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
(* 	$Id: cgi_fast.ml,v 1.14 2006/01/08 22:20:47 chris_77 Exp $	 *)

(* FastCGI protocol (version 1) according to the specification found
   at http://www.fastcgi.com/devkit/doc/fcgi-spec.html

   The code is thread safe. *)

open Cgi_common
open Cgi_types

let fcgi_version = '\001'
let fcgi_listensock = Unix.stdin

(* Values for type component of FCGI_Header *)
let fcgi_begin_request = '\001'
let fcgi_abort_request = '\002'
let fcgi_end_request   = '\003'
let fcgi_params        = '\004'
let fcgi_stdin         = '\005'
let fcgi_stdout        = '\006'
let fcgi_stderr        = '\007'
let fcgi_data          = '\008'
let fcgi_get_values        = '\009'
let fcgi_get_values_result = '\010'
let fcgi_unknown_type      = '\011'

(* Mask for flags component of FCGI_BeginRequestBody *)
let fcgi_keep_conn = 0x1

(* Get the list of valid IP addresses for the Web server or [] if not set. *)
let fcgi_web_server_addrs =
  try
    let addrs = rev_split ',' (Unix.getenv "FCGI_WEB_SERVER_ADDRS") in
    List.map Unix.inet_addr_of_string addrs
  with
    Not_found | Failure _ -> []


type record = {
  version : int;
  ty : char;
  rec_id : int;
  data : string;
}

(* Values for protocolStatus component of FCGI_EndRequestBody *)
type protocol_status =
  | REQUEST_COMPLETE
  | CANT_MPX_CONN
  | OVERLOADED
  | UNKNOWN_ROLE


(* Output functions
 ***********************************************************************)

(* [output ty fd id s ofs len] send on [fd] for a request [id], a
   record of type [ty] whose data is the substring [s.[ofs .. ofs +
   len - 1]] where [len <= 65535].  [ty] can be [fcgi_stdout],
   [fcgi_stderr], [fcgi_get_values_result] or [fcgi_unknown_type]. *)
let output ty fd id s ofs len =
  let padding_len = 8 - len mod 8 in
  (* We keep total size a multiple of 8 bytes so the web server can
     easily align the data for efficency. *)
  let r = String.create 8 in
  r.[0] <- fcgi_version;
  r.[1] <- ty;
  r.[2] <- Char.chr(id lsr 8);
  r.[3] <- Char.chr(id land 255); (* requestId *)
  r.[4] <- Char.chr(len lsr 8);
  r.[5] <- Char.chr(len land 255); (* contentLength *)
  r.[6] <- Char.chr(padding_len);
  ignore(Unix.write fd r 0 8);
  ignore(Unix.write fd s ofs len);
  ignore(Unix.write fd r 0 padding_len) (* Padding (garbage) *)

(* [output_string_unsafe ty fd id s] send on [fd] for a request [id], a
   record of type [ty] whose data is [s].  It is assumed that
   [String.length s <= 65535]. *)
let output_string_unsafe ty fd id s =
  output ty fd id s 0 (String.length s)

(* [output_string ty fd id s] does the same as [output_string_unsafe]
   except that empty strings are not outputted (they would close the
   stream) and strings longer than 65535 bytes are sent in several
   records. *)
let output_string =
  let rec send_range out s ofs len =
    if len <= 0xFFFF then out s ofs len
    else begin
      out s ofs 0xFFFF;
      send_range out s (ofs + 0xFFFF) (len - 0xFFFF)
    end in
  fun ty fd id s ->
    if s <> "" then send_range (output ty fd id) s 0 (String.length s)


let send_end_request fd id status exit_code =
  let r = String.create 16 in
  r.[0] <- fcgi_version;
  r.[1] <- fcgi_end_request;
  r.[2] <- Char.chr(id lsr 8);
  r.[3] <- Char.chr(id land 255); (* requestId *)
  r.[4] <- '\000';
  r.[5] <- '\008'; (* contentLength = 8 *)
  r.[6] <- '\000'; (* no padding *)
  r.[11] <- Char.chr(exit_code land 255); (* appStatus (4 bytes) *)
  let exit_code = exit_code lsr 8 in
  r.[10] <- Char.chr(exit_code land 255);
  let exit_code = exit_code lsr 8 in
  r.[9] <- Char.chr(exit_code land 255);
  r.[8] <- Char.chr(exit_code lsr 8);
  r.[12] <- Char.unsafe_chr(match status with
			    | REQUEST_COMPLETE -> 0
			    | CANT_MPX_CONN ->    1
			    | OVERLOADED ->       2
			    | UNKNOWN_ROLE ->     3); (* protocolStatus *)
  ignore(Unix.write fd r 0 16)

let send_unknown_type fd t =
  let r = String.create 16 in
  r.[0] <- fcgi_version;
  r.[1] <- fcgi_unknown_type;
  r.[2] <- '\000';
  r.[3] <- '\000'; (* requestId = 0 *)
  r.[4] <- '\000';
  r.[5] <- '\008'; (* contentLength = 8 *)
  r.[6] <- '\000'; (* no padding *)
  r.[8] <- t; (* type *)
  ignore(Unix.write fd r 0 16)


let set_length4 s ofs n =
  (* 4 bytes encoding of the length [n] *)
  s.[ofs+3] <- Char.unsafe_chr(n land 0xFF);
  let n = n lsr 8 in
  s.[ofs+2] <- Char.unsafe_chr(n land 0xFF);
  let n = n lsr 8 in
  s.[ofs+1] <- Char.unsafe_chr(n land 0xFF);
  s.[ofs] <- Char.chr((n lsr 8) lor 0x80)

(* [lengths_of_key_val k v] returns a string encoding the lengths
   of the key-value pair [(k,v)] according to fcgi spec: 3.4
   Name-Value Pairs. *)
let lengths_of_key_val k v =
  let klen = String.length k
  and vlen = String.length v in
  if klen < 128 then
    if vlen < 128 then begin
      let s = String.create 2 in
      s.[0] <- Char.chr klen;
      s.[1] <- Char.chr vlen;
      s
    end
    else begin
      let s = String.create 5 in
      s.[0] <- Char.chr klen;
      set_length4 s 1 vlen;
      s
    end
  else
    if vlen < 128 then begin
      let s = String.create 5 in
      set_length4 s 0 klen;
      s.[4] <- Char.chr vlen;
      s
    end
    else begin
      let s = String.create 8 in
      set_length4 s 0 klen;
      set_length4 s 4 vlen;
      s
    end

(* Input functions
 ***********************************************************************)

let rec really_read_aux fd buf ofs len =
  let r = Unix.read fd buf ofs len in
  if r < len then really_read_aux fd buf (ofs + r) (len - r)
  else buf

let really_read fd len =
  really_read_aux fd (String.create len) 0 len

(* Padding is at most 256 bytes long and we do not care about thread
   safety (since we read garbage anyway), so we hoist the buffer
   outside the function. *)
let padding_buffer = String.create 256
let rec read_padding fd len =
  let r = Unix.read fd padding_buffer 0 len in
  if r < len then read_padding fd (len - r)


(* [input_record fd] returns the next record from the socket [fd]. *)
let input_record fd =
  let header = really_read fd 8 in
  let version = Char.code(header.[0])
  and id =  Char.code(header.[2]) lsl 8 + Char.code(header.[3])
  and len = Char.code(header.[4]) lsl 8 + Char.code(header.[5])
  and padding = Char.code(header.[6]) in
  let data = really_read fd len in
  read_padding fd padding;
  { version = version;
    ty = header.[1];
    rec_id = id;
    data = data }


(* [get_length data ofs] returns the length at offset [ofs] encoded
   according to the Name-Value Pairs specs or raise [Failure].  It is
   assumed that [0 <= ofs < String.length s]. *)
let get_length data ofs =
  let b = Char.code(data.[ofs]) in
  if b lsr 7 = 0 then
    (b, ofs + 1)
  else begin
    if ofs + 3 >= String.length data then failwith "add_params";
    let b2 = Char.code(data.[ofs + 1])
    and b1 = Char.code(data.[ofs + 2])
    and b0 = Char.code(data.[ofs + 3]) in
    (((b land 0x7F) lsl 24) + (b2 lsl 16) + (b1 lsl 8) + b0, ofs + 4)
  end

(* [add_params h data] adds to the hash [h] the key-value pairs
   contained in [data].  The keys are uppercased (CGI/1.1 says they
   must be treated case insensitively).

   @raise Failure if the key or val lengths exceed the length of the
   string [data]. *)
let add_params =
  let rec add data ofs datalen h =
    if ofs < datalen then begin
      let namelen, ofs = get_length data ofs in
      if ofs >= datalen then failwith "add_params";
      let valuelen, ofs = get_length data ofs in
      let ofs_value = ofs + namelen in
      let ofs_next = ofs_value + valuelen in
      if  ofs_next > datalen then failwith "add_params";
      let name = String.uppercase(String.sub data ofs namelen) in
      Hashtbl.add h name (String.sub data ofs_value valuelen);
      add data ofs_next datalen h
    end in
  fun h data -> add data 0 (String.length data) h


(* Management Record Types
 ***********************************************************************)

let add_key_val buf k v =
  Buffer.add_string buf (lengths_of_key_val k v);
  Buffer.add_string buf k;
  Buffer.add_string buf v

let handle_get_values fd record ~max_conns ~max_reqs ~multiplex =
  let vars = Hashtbl.create 3 in
  (try add_params vars record.data
   with Failure _ -> ());
  let buf = Buffer.create 80 in
  if Hashtbl.mem vars "FCGI_MAX_CONNS" then
    add_key_val buf "FCGI_MAX_CONNS" (string_of_int max_conns);
  if Hashtbl.mem vars "FCGI_MAX_REQS" then
    add_key_val buf "FCGI_MAX_REQS" (string_of_int max_reqs);
  if Hashtbl.mem vars "FCGI_MPXS_CONNS" then
    add_key_val buf "FCGI_MPXS_CONNS" (if multiplex then "1" else "0");
  output_string_unsafe fcgi_get_values_result fd 0 (Buffer.contents buf)




(* Dealing with a given connection
 ***********************************************************************)

module Connection =
struct
  let add_request conn id version role flags =
    if Hashtbl.mem conn.requests id then
      (* Previous request still exist, cannot accept a new one! *)
      raise Ignore_record
    else begin
      let stdout = Buffer.create 1024
      and stderr = Buffer.create 160 in
      Hashtbl.add conn.requests id
	{ role = role;
	  gateway = FCGI version;
	  metavars = Hashtbl.create 18;
	  params = Hashtbl.create 10;
	  is_multipart = false;
	  uploads = Hashtbl.create 1;
	  print_string = Buffer.add_string stdout;
	  prerr_string = Buffer.add_string stderr;
	  header_not_emitted = true;
	  access = Mutex.create();
	  stdout = stdout;
	  stderr = stderr;
	  (* FCGI management *)
	  id = id;
	  keep_conn = flags land fcgi_keep_conn <> 0;
	  status = Get_params;
	  buf = Buffer.create 0xFFFF;
	  abort = false;
	}
    end

  (* Try to flush all outputs -- but do not insist if it is locked by
     a thread processing a request. *)
  let try_flush conn =
    Hashtbl.iter (fun id req ->
		    if Mutex.try_lock req.access then begin
		      output_string fcgi_stdout conn.fd id
			(Buffer.contents req.stdout);
		      Buffer.clear req.stdout;
		      output_string fcgi_stderr conn.fd id
			(Buffer.contents req.stderr);
		      Buffer.clear req.stderr;
		      Mutex.unlock req.access;
		    end
		 ) conn.requests

  let free_request conn id =
    Hashtbl.remove conn.requests id

  (* Flush and close output streams, then free the ressources.  We
     suppose the process handling is request has ended so it will not
     try to perform writes. *)
  let close_request conn id =
    let req = Hashtbl.find conn.requests id in
    output_string fcgi_stdout conn.fd id (Buffer.contents req.stdout);
    output_string_unsafe fcgi_stdout conn.fd id "";
    output_string fcgi_stderr conn.fd id (Buffer.contents req.stderr);
    output_string_unsafe fcgi_stderr conn.fd id "";
    free_request conn id

  (* Send the error message [err_msg] and free the request. *)
  let close_request_error conn req errn err_msg =
    if req.header_not_emitted then
      req.print_string(Printf.sprintf
			 "Status: %i %s\r\nContent-Type: text/html\r\n\r\n"
			 errn err_msg);
    let email =
      try Hashtbl.find req.metavars "SERVER_ADMIN"
      with Not_found ->
	try Unix.getenv "SERVER_ADMIN" (* win32 set this env var *)
	with Not_found -> "webmaster@" ^ (Unix.gethostname()) in
    req.print_string(error_html errn err_msg email);
    close_request conn req.id
end

let close_no_error fd =
  try Unix.close fd with _ -> ()


(* [handle_request_error conn f request] apply [f] to [request] and
   handle the exceptions launched by [f] in a protocol dependent way
   (meant to be used as [request.handle_error f request]). *)
let handle_request_error conn f request =
  let exit_code =
    try
      f request;
      Connection.close_request conn request.id;
      0
    with
    | Exit ->
	if request.header_not_emitted then
	  request.print_string "Status: 204 No Response\r\n\r\n";
	0
    | Abort ->
	request.prerr_string "Exception \"Abort\" not caught.";
	Connection.close_request_error conn request cHTTP_PARTIAL_CONTENT
	  "Script aborted";
	1
    | HttpError e ->
	request.prerr_string("Exception \"HttpError("
			     ^ string_of_int e ^ ")\".");
	Connection.close_request_error conn request e
	  ("HTTP Error " ^ string_of_int e);
	1
    | e ->
	request.prerr_string("Uncaught exception " ^ Printexc.to_string e);
	Connection.close_request_error conn request
	  cHTTP_INTERNAL_SERVER_ERROR "Internal Server Error";
	1 in
  send_end_request conn.fd request.id REQUEST_COMPLETE exit_code;
  if not request.keep_conn then
    (* Hopefully, when we have to close the connection, there will
       be only one request sent. *)
(     Unix.close conn.fd
(*       ; Printf.eprintf "Closed id = %i (%i)\n" request.id (Obj.magic conn.fd); flush stderr; *)
      )




(* Listen to incoming records on [fd] and build requests from them.
   It is the responsability of this function to manage multiplexed
   connections and to distribute incoming data.  *)
let handle_requests fork f conn =
  let handle_record record =
    begin match record.ty with
    | '\001' -> (* BEGIN_REQUEST ------------------------------------- *)
	if String.length record.data <> 8 then raise Ignore_record;
(* Printf.eprintf "Begin  id = %i (%i)\n" record.rec_id (Obj.magic conn.fd); flush stderr; *)
          if Hashtbl.length conn.requests < conn.max_reqs then
	  let role =
	    Char.code(record.data.[0]) lsl 8 + Char.code(record.data.[1])
	  and flags = Char.code(record.data.[2]) in
	  begin match role with
	  | 1 -> Connection.add_request conn record.rec_id record.version
	      Responder flags
	  | 2 -> Connection.add_request conn record.rec_id record.version
	      Authorizer flags
	  | 3 -> Connection.add_request conn record.rec_id record.version
	      Filter flags
	  | _ -> (* Rejecting this request that has an unknown role. *)
	      send_end_request conn.fd record.rec_id UNKNOWN_ROLE cHTTP_OK
	  end
	else (* More requests that specified. *)
	  send_end_request conn.fd record.rec_id OVERLOADED cHTTP_OK

    | '\002' -> (* ABORT_REQUEST ------------------------------------- *)
	let request =
	  try Hashtbl.find conn.requests record.rec_id
	  with Not_found (* non existing req *) -> raise Ignore_record in
	if request.status <> Handler_launched then begin
	  (* The request is not complete, this library frees it. *)
	  Connection.free_request conn record.rec_id;
	  send_end_request conn.fd record.rec_id REQUEST_COMPLETE cHTTP_OK;
	end
	else (* The spec says that the return code should be truly from
		the application.  So we just set a flag in the request
		that every CGI method will first check so that it raises
		[Abort] if true -- thus the CGI program can deal with it. *)
	  request.abort <- true

    | '\004' -> (* FCGI_PARAMS --------------------------------------- *)
	let request =
	  try Hashtbl.find conn.requests record.rec_id
	  with Not_found (* non existing req *) -> raise Ignore_record in
	if request.status <> Get_params then raise Ignore_record;
	if String.length record.data > 0 then
	  try add_params request.metavars record.data
	  with Failure _ ->
	    Connection.close_request_error conn request cHTTP_BAD_REQUEST
	      "Wrong length of FCGI_PARAMS"
	else begin
	  (* PARAMS stream closed *)
	  match request.role with
	  | Authorizer ->
	      request.status <- Handler_launched;
	      fork (handle_request_error conn f) request (* launch *)
	  | Responder ->
	      (* Check whether the params are enough to launch the cgi *)
	      let rmethod = metavar_string request "REQUEST_METHOD" in
	      begin match String.uppercase rmethod with
	      | "GET" | "HEAD" ->
		  let qs = metavar_string request "QUERY_STRING" in
		  parse_query qs request.params;
		  request.status <- Handler_launched; (* => STDIN ignored *)
		  fork (handle_request_error conn f) request (* launch *)
	      | "POST" -> request.status <- Get_stdin
	      | "PUT"
	      | "DELETE"
	      | "OPTIONS"
	      | "TRACE"
	      | _ ->
		  Connection.close_request_error conn request
		    cHTTP_METHOD_NOT_ALLOWED
		    ("Unsupported REQUEST_METHOD: " ^ rmethod)
	      end
	  | Filter -> request.status <- Get_stdin
	end

    | '\005' -> (* FCGI STDIN ---------------------------------------- *)
	let request =
	  try Hashtbl.find conn.requests record.rec_id
	  with Not_found (* non existing req *) -> raise Ignore_record in
	if request.status <> Get_stdin then raise Ignore_record;
	if String.length record.data > 0 then
	  (* FIXME: add max length check *)
	  Buffer.add_string request.buf record.data
	else
	  (* stdin stream closed -- process it according to CGI/1.1 *)
	  let len = metavar_int request "CONTENT_LENGTH" ~default:0 in
	  if Buffer.length request.buf <> len then
	    Connection.close_request_error conn request cHTTP_BAD_REQUEST
	      "CONTENT_LENGTH <> length STDIN data"
	  else begin
	    try
	      parse_post_data (Buffer.contents request.buf) request;
	      Buffer.clear request.buf; (* for DATA now *)
	      match request.role with
	      | Authorizer -> assert false (* never has status = Get_stdin *)
	      | Responder ->
		  request.status <- Handler_launched;
		  fork (handle_request_error conn f) request (* launch *)
	      | Filter -> request.status <- Get_data
	    with Unsupported_media_type t ->
	      Connection.close_request_error conn request
		cHTTP_UNSUPPORTED_MEDIA_TYPE ("Unsupported media type: " ^ t)
	  end

    | '\008' -> (* DATA -- only for the filter role ------------------ *)
	let request =
	  try Hashtbl.find conn.requests record.rec_id
	  with Not_found (* non existing req *) -> raise Ignore_record in
	if request.status <> Get_data then raise Ignore_record;
	if String.length record.data > 0 then
	  Buffer.add_string request.buf record.data
	else begin
	  (* data stream closed -- process it *)
	  let len = metavar_int request "FCGI_DATA_LENGTH" ~default:0 in
	  if Buffer.length request.buf <> len then begin
	    Buffer.clear request.buf;
	    raise Ignore_record;
	  end;
	  fork (handle_request_error conn f) request (* launch *)
	    
	end

    | '\009' -> (* GET_VALUES -- handled by this library ------------- *)
	handle_get_values conn.fd record ~max_conns:conn.max_conns
	  ~max_reqs:conn.max_reqs ~multiplex:(conn.max_reqs > 1)

    | _ -> (* UNKNOWN_TYPE ------------------------------------------- *)
	send_unknown_type conn.fd record.ty
    end;
    Connection.try_flush conn;
  in
  try
    while true do
      try  handle_record(input_record conn.fd)
      with Ignore_record -> () (* Ignore the record if invalid *)
    done
  with
  | Unix.Unix_error(_, _, _) -> close_no_error conn.fd
      (* Likely the connection has been closed, by the server or by us
	 to signify a end-of-request.  (Still, we are not sure, so we
	 try to close it again.)  The CGI script must not end, just
	 wait for the next connection. *)



(* Establish server
 ***********************************************************************)

let establish_server_socket sock ~max_conns ~max_reqs (f:connection -> unit) =
  Unix.listen sock 5;
  while true do
    let (fd, server) = Unix.accept sock in
    (* If [fcgi_web_server_addrs] is set, the connection must come from
       one of the specified IP addesses, if not one closes the
       connection immediately. (3.2 Accepting Transport Connections) *)
    match server with
    | Unix.ADDR_UNIX _ ->
	if fcgi_web_server_addrs = []
	then f { fd = fd;
		 max_conns = max_conns;
		 max_reqs = max_reqs;
		 handle_requests = handle_requests;
		 requests = Hashtbl.create max_reqs
	       }
	else Unix.close fd (* Connection refused *)
    | Unix.ADDR_INET(addr,_) ->
	if fcgi_web_server_addrs = [] || List.mem addr fcgi_web_server_addrs
	then f { fd = fd;
		 max_conns = max_conns;
		 max_reqs = max_reqs;
		 handle_requests = handle_requests;
		 requests = Hashtbl.create max_reqs
	       }
	else Unix.close fd (* Connection refused *)
  done

(* Under M$win, the web server communicates with a FCGI script that it
   launches by means of a named pipe [fd] (contrarily to the spec).
   The requests are all sent through that pipe.  Thus there is a
   single connection. *)
let establish_server_pipe fd ~max_conns ~max_reqs (f:connection -> unit) =
  f { fd = fd;
      max_conns = max_conns;
      max_reqs = max_reqs;
      handle_requests = handle_requests;
      requests = Hashtbl.create max_reqs
    }


(* If the socket connection is closed on the client end, the SIGPIPE
   signal will be triggered, aborting the program.  We want to see the
   unix error, so disable the signal (if it exists for the OS). *)
let () =
  try Sys.set_signal Sys.sigpipe Sys.Signal_ignore
  with Invalid_argument _ -> ()

let () =
  try Sys.set_signal Sys.sigterm (Sys.Signal_handle(fun _ -> exit 0))
  with Invalid_argument _ -> ()
