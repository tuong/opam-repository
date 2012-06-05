(* File: camlGI.mli

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
(* 	$Id: camlGI.mli,v 1.8 2005/06/12 21:44:22 chris_77 Exp $	 *)

(** Library for writing (F)CGI programs.

    CamlGI supports multiple connections and multiplexed requests.
    You can execute those concurrently with the model of your choice
    (unix fork, threads,...).

    This library is mostly compatible with the Cgi library of mod_caml
    so porting programs between the two is fairly straightforward.
*)


(** Generate and parse cookies *)
module Cookie :
sig
  class cookie :
    name:string -> value:string -> max_age:int option ->
    domain:string -> path:string -> secure:bool ->
  object
    method name : string
      (** Return the name of the cookie. *)
    method value : string
      (** Return the value of the cookie. *)
    method max_age : int option
      (** Lifetime of the cookie in seconds. *)
    method domain : string
      (** Return the domain of the cookie, or "" if not set. *)
    method path : string
      (** Return the path of the cookie, or "" if not set. *)
    method secure : bool
      (** Return true if the cookie is a secure cookie. *)

    method set_name : string -> unit
      (** Set the name of the cookie. *)
    method set_value : string -> unit
      (** Set the value of the cookie. *)
    method set_max_age : int option -> unit
      (** [#set_max_age (Some s)] set the lifetime of the cookie to
	  [s] seconds [s].  [#set_max_age None] means that the cookie
	  will be discarded when the client broser exits. *)
    method set_domain : string -> unit
      (** Set the domain of the cookie. *)
    method set_path : string -> unit
      (** Set the path of the cookie. *)
    method set_secure : bool -> unit
      (** Set the cookie as a secure cookie.  Secure cookies are only
	  transmitted to HTTPS servers. *)

    method to_string : string
      (** Return the string representation of the cookie. *)
  end

  val cookie : ?max_age:int -> ?domain:string -> ?path:string ->
    ?secure:bool -> string -> string -> cookie
    (** [cookie ?expires ?domain ?path name value] creates a cookie
	with name [name] and value [value].

	@param max_age lifetime of the cookie in seconds (default: none).
	@param domain  domain of the cookie (default: "").
	@param path    path of the cookie (default: "").
	@param secure  whether the cookie is secure (default: [false]).
    *)

  val parse : string -> cookie list
    (** [parse header] parse zero or more cookies.  Normally [header]
	comes from a "Cookie: header" field. *)
end


(** {2 Main CGI module} *)

(** (F)CGI high level functions *)
module Cgi :
sig
  exception HttpError of int

  module Request :
  sig
    type t
      (** Type representing the information contained in one request
	  of the web server. *)

    (** Possible roles of the CGI script *)
    type role =
      | Responder (** Receive the information associated with an HTTP
		      request and generates an HTTP response. *)
      | Authorizer (** Receive the information associated with an HTTP
		       request and generates an (un)authorized
		       decision. *)
      | Filter (** Receive the information associated with an HTTP
		   request plus an extra stream of data and generates a
		   filtered version of the data stream as an HTTP
		   response. *)

    (** CGI or FCGI script *)
    type gateway =
      | CGI of int * int
      | FCGI of int

    val gateway : t -> gateway
      (** The type and version of the CGI used. *)
    val role : t -> role
      (** The role of the script. *)

    val path_info : t -> string
      (** Returns the PATH_INFO, that is the portion of the URI
	  following the script name but preceding the query data.  "/"
	  represent a single void path segment.  The CGI specifications
	  recommend to return "404 Not Found" if path_info <> "" but is
	  not used. *)
    val protocol : t -> string
      (** The protocol of the request, in uppercase.  E.g. "HTTP/1.1". *)
    val remote_addr : t -> string
      (** The IP adress of the client making the request.  Note it can
	  be the one of a proxy in the middle. *)
    val server_name : t -> string
      (** Name of the server, derived from the host part of the script URI. *)
    val server_port : t -> int
      (** The port on which the request was received. *)
    val server_software : t -> string
      (** The name and version of the web server software answering the
	  request. *)

    val accept : t -> string
      (** Returns the list of accepted MIME types by the client. *)
    val accept_charset : t -> string
      (** Return a list of charset supported by the client. *)
    val accept_encoding : t -> string
      (** List of encodings supported by the client. *)
    val auth : t -> string
      (** The HTTP authentication scheme.  E.g. "Basic".  See section 11
	  of the HTTP/1.1 specification for more details. *)
    val user : t -> string
      (** The user-ID supplied when [auth r = "Basic"]. *)
    val user_agent : t -> string
      (** The identification of the client browser. *)

    val metavar : t -> string -> string
      (** [metavar r name] returns the value of the CGI metavariable
	  [name] for the request [r].  (Remember that CGI does not
	  distinguish between nonexisting arguments and arguments with
	  value [""].) *)

    val print_string : t -> string -> unit
    val prerr_string : t -> string -> unit
  end


  (** {3 Setting up the application server} *)

  type connection

  val establish_server : ?max_conns:int -> ?max_reqs:int ->
    ?sockaddr:Unix.sockaddr -> ?post_max:int ->
    (connection -> unit) -> unit
    (** [establish_server ?max_conns ?max_reqs ?sockaddr ?post_max f]
	starts a server listening on the socket appropriate for CGI or
	FCGI and, for each accepted connection [conn], executes [f
	conn].  The exceptions possibly raised by [f] are not caught
	by [establish_server].  It is no problem that [f] starts a new
	thread to handle the connection (and thus returns immediately).

	@param max_conns is the maximum of connections the web server
	can make to this script.  By default, each connection is
	processed sequentially, so the default value for [max_conns]
	is [1].  If you start processes or threads to handle
	connections, it is your responsability not to accept more than
	[max_conns] connections.  The value of [max_conns] only serves
	to inform the web server about the capabilities of the FCGI
	script.

	@param max_reqs is the maximum of requests a web server can
	multiplex through a given connection.  Again, if you start
	processes ot threads to handle requests, it is your
	responsability to limit the number of them.  [max_reqs] is
	only used to inform the web server about how many requests it
	can multiplex on a given connection.  Beware that if you set
	[max_reqs] to [1] but have threads handling different requests
	of a given connection, the outputs may mix-up (thus be
	incorrect).

	@param sockaddr the unix or the TCP/IP socket that the script
	will use to communicate with the web server.  Setting this
	implies that the script uses the FCGI protocol.  By default,
	on uses what is appropriate for the CGI OR FCGI protocol.  For
	example, if your script is listening on port 8888 on a
	possibly remote machine, you can use
	[Unix.ADDR_INET(Unix.inet_addr_any, 8888)].

	@param post_max set the maximum size for POSTed requests in
	bytes.  This is a security feature to prevent clients from
	overrunning the server with data.  The default is
	[Sys.max_string_length], meaning no limit (besides OCaml
	ones).

	For FastCGI, the environment variable FCGI_WEB_SERVER_ADDRS
	may be used to specify a coma separated list of IP addresses
	from which the web server can connect.  If not set, any
	address is accepted.  *)

  val handle_requests : ?fork:((Request.t -> unit) -> Request.t -> unit) ->
    (Request.t -> unit) -> connection -> unit
    (** [handle_requests ?fork f conn] listen on the connection [conn]
	for requests.  For each completed request [req], it executes
	[fork f req].

	@param fork the function that starts a new process or thread.
	The default is to execute [f] and only after continue to
	listen for more requests.

	Exceptions thrown by [f] are caught (so the possible thread
	executing [f] will not be terminated by these).  The exception
	[Exit] is caught and ignored (this is considered a valid way
	of ending a script).  {!CamlGI.Cgi.HttpError} exceptions are
	turned into appropriate error codes.  All other exceptions
	provoke a internal server error and are logged in the server
	error log.

	Note that the exceptions raised by [fork] are NOT caught. *)

  val register_script : ?sockaddr:Unix.sockaddr -> (Request.t -> unit) -> unit
    (** Scripts must call [register_script f] once to register their
	main function [f].  This should be called last (nothing that
	follows will be executed).  The data is buffered and may not
	be fully written before [f] ends.

	This is actually a convenience function that sets up a server (with
	[establish_server]) and processes (through [handle_requests])
	all connections and requests sequentially -- i.e. no
	fork/thread. *)


  (** {3 Cgi} *)

  (** Type of acceptable template objects. *)
  class type template =
  object
    method output : (string -> unit) -> unit
      (** [#output print] must use the [print] function to output the
	  template (with the necessary substitutions done,...). *)
  end

  exception Abort
    (** Exception raised by all terminating cgi methods if the server
	requires to abort the request. *)

  (** Type returned by [cgi#upload] method. *)
  type upload_data = {
    upload_value: string;
    upload_filename: string;
    upload_content_type: string
  }

  (** [new cgi r] creates a cgi object for the request [r].  Note that
      you are advised not to create more than one cgi per request
      unless you know what you are doing.  *)
  class cgi : Request.t ->
  object
    method header : ?content_type:string -> ?cookie:Cookie.cookie ->
      ?cookies:Cookie.cookie list -> ?cookie_cache:bool -> unit -> unit
      (** Emit the header. The default content type is "text/html". *)

    method template : 'a. ?content_type:string -> ?cookie:Cookie.cookie ->
      ?cookies:Cookie.cookie list -> ?cookie_cache:bool ->
      (#template as 'a) -> unit
      (** Emit the header (unless #header was issued before) followed by
	  the given template.  @raise Failure if the output is not
	  successful. *)

    method exit : unit -> 'a
    (** Exit the current cgi script.  (You need not to call this at
	the end of your code, just if you want to exit earlier.)  *)

    method redirect : ?cookie:Cookie.cookie -> ?cookies:Cookie.cookie list ->
      ?cookie_cache:bool -> string -> 'a
      (** [#redirect ?cookie ?cookies url] quits the current cgi
	  script and send to the client a redirection header to [url]. *)

    method url : unit -> string
      (** Return the URL of the script. *)

    method param : string -> string
    (** [#param name] returns the "first" value of the parameter
	[name].  @raise Not_found if [name] does not designate a valid
	parameter. *)

    method param_all : string -> string list
      (** [#param_all name] returns all the values of the parameter
	  [name].  @raise Not_found if [name] does not designate a
	  valid parameter. *)

    method param_exists : string -> bool
      (** Return true iff the named parameter exists. *)

    method param_true : string -> bool
    (** This method returns false if the named parameter is missing,
	is an empty string, or is the string ["0"]. Otherwise it
	returns true. Thus the intent of this is to return true in the
	Perl sense of the word.  If a parameter appears multiple
	times, then this uses the first definition and ignores the
	others. *)

    method params : (string * string) list
      (** Return an assoc-list mapping name -> value for all parameters.
	  Note that CGI scripts may have multiple values for a single name. *)

    method is_multipart : bool
      (** Returns true iff the request was a [multipart/form-data]
	  [POST] request.  Such requests are used when you need to
	  upload files to a CGI script.  *)

    method upload : string -> upload_data
      (** For multipart forms only.  Returns the full upload data passed
	  by the browser for a particular parameter.
	  @raise Not_found is no such parameter exists.  *)

    method upload_all : string -> upload_data list
      (** For multipart forms only.  As for [#upload], but returns all
	  uploads.  *)

    method cookie : string -> Cookie.cookie
      (** Return the named cookie, or throw [Not_found]. *)

    method cookies : Cookie.cookie list
      (** Return a list of all cookies passed to the script. *)

    method log : string -> unit
      (** [log s] Log the message [s] into the webserver log. *)

    method request : Request.t
      (** Returns the original request object (passed in the constructor). *)
  end


  val random_sessionid : unit -> string
    (** Generates a 128 bit (32 hex digits) random string which can be
	used for session IDs, random cookies and so on.  The string
	returned will be very random and hard to predict, at least if
	your platform possesses /dev/urandom *)


  module Cgi_args :
  sig
    val parse : string -> (string * string) list
      (** [parse qs] parses up a standard CGI query string (such as
	  ["a=1&b=2"]), and returns the list of name, value pairs.  This
	  is a helper function.  The [cgi] class does this parsing for
	  you, so you shouldn't need to use this function unless you're
	  doing something out of the ordinary.  *)

    val make : (string * string) list -> string
      (** [make bindings] returns a query string from the list
	  [bindings] of name, value pairs.  For example, [make
	  [("a", "1"); ("b", "2")]] returns ["a=1&b=2"]. *)
  end


  (** {3 Apache names for HTTP errors} *)

  val cHTTP_CONTINUE : int
  val cHTTP_SWITCHING_PROTOCOLS : int
  val cHTTP_PROCESSING : int
  val cHTTP_OK : int
  val cHTTP_CREATED : int
  val cHTTP_ACCEPTED : int
  val cHTTP_NON_AUTHORITATIVE : int
  val cHTTP_NO_CONTENT : int
  val cHTTP_RESET_CONTENT : int
  val cHTTP_PARTIAL_CONTENT : int
  val cHTTP_MULTI_STATUS : int
  val cHTTP_MULTIPLE_CHOICES : int
  val cHTTP_MOVED_PERMANENTLY : int
  val cHTTP_MOVED_TEMPORARILY : int
  val cHTTP_SEE_OTHER : int
  val cHTTP_NOT_MODIFIED : int
  val cHTTP_USE_PROXY : int
  val cHTTP_TEMPORARY_REDIRECT : int
  val cHTTP_BAD_REQUEST : int
  val cHTTP_UNAUTHORIZED : int
  val cHTTP_PAYMENT_REQUIRED : int
  val cHTTP_FORBIDDEN : int
  val cHTTP_NOT_FOUND : int
  val cHTTP_METHOD_NOT_ALLOWED : int
  val cHTTP_NOT_ACCEPTABLE : int
  val cHTTP_PROXY_AUTHENTICATION_REQUIRED : int
  val cHTTP_REQUEST_TIME_OUT : int
  val cHTTP_CONFLICT : int
  val cHTTP_GONE : int
  val cHTTP_LENGTH_REQUIRED : int
  val cHTTP_PRECONDITION_FAILED : int
  val cHTTP_REQUEST_ENTITY_TOO_LARGE : int
  val cHTTP_REQUEST_URI_TOO_LARGE : int
  val cHTTP_UNSUPPORTED_MEDIA_TYPE : int
  val cHTTP_RANGE_NOT_SATISFIABLE : int
  val cHTTP_EXPECTATION_FAILED : int
  val cHTTP_UNPROCESSABLE_ENTITY : int
  val cHTTP_LOCKED : int
  val cHTTP_FAILED_DEPENDENCY : int
  val cHTTP_INTERNAL_SERVER_ERROR : int
  val cHTTP_NOT_IMPLEMENTED : int
  val cHTTP_BAD_GATEWAY : int
  val cHTTP_SERVICE_UNAVAILABLE : int
  val cHTTP_GATEWAY_TIME_OUT : int
  val cHTTP_VERSION_NOT_SUPPORTED : int
  val cHTTP_VARIANT_ALSO_VARIES : int
  val cHTTP_INSUFFICIENT_STORAGE : int
  val cHTTP_NOT_EXTENDED : int

  val cDOCUMENT_FOLLOWS : int
  val cPARTIAL_CONTENT : int
  val cMULTIPLE_CHOICES : int
  val cMOVED : int
  val cREDIRECT : int
  val cUSE_LOCAL_COPY : int
  val cBAD_REQUEST : int
  val cAUTH_REQUIRED : int
  val cFORBIDDEN : int
  val cNOT_FOUND : int
  val cMETHOD_NOT_ALLOWED : int
  val cNOT_ACCEPTABLE : int
  val cLENGTH_REQUIRED : int
  val cPRECONDITION_FAILED : int
  val cSERVER_ERROR : int
  val cNOT_IMPLEMENTED : int
  val cBAD_GATEWAY : int
  val cVARIANT_ALSO_VARIES : int
end


(** {2 Useful modules to write scripts} *)

(** Template system compatible with mod_caml one. *)
module Template :
sig
  type var =
    | VarString of string			(** ::tag:: *)
    | VarTable of table_row list		(** ::table(tag):: *)
    | VarConditional of bool			(** ::if(tag):: *)
    | VarCallback of (string list -> string)	(** ::call(f, x1,...):: *)
  and table_row = (string * var) list

  (** Variables are either simple string, tables, conditionals or callbacks.

      A simple string is set with [template#set "name" s] where [s] will
      be automatically escaped depending on the declaration in the template:

      - [::name::] does no escaping;
      - [::name_url::] escapes for URL encoding, make it suitable in a
        link [<a href="::name_url::">];
      - [::name_html::] escapes for HTML display of the string;
      - [::name_html_tag::] escapes the string to make it suitable to be
        placed between quotes in an HTML tag, e.g.
        [<input value="::name_html_tag::">];
      - [::name_html_textarea::] escapes the string to make it suitable
        to be placed between [<textarea>...</textarea>].

      Tables are declared in the template by [::table(name)::] {i row
      template} [::end::].  The {i row template} can contain other
      variables.  Calling [template#table "name" rows], where [rows] is
      a list [[row1, row2,...,rowN]], will insert [N] {i row templates}
      with each template having its variables set thanks to [row1],...
      each of which is an associative list name -> value (of type
      {!Template.table_row}).

      Conditionals are declared in the template by [::if(name)
      .. ::else:: .. ::end::] with the "else" clause being optional.
      Calling [template#conditional] sets up a conditional value.

      Calling [template#callback "fname" f] sets up the callback
      function declared by [::call(fname,arg1,...,argN)::] replacing the
      call by the value of [f] applied to the list [[arg1,...,argN]].
      The string returned by [f] can be escaped by using suffices in the
      template as for simple tags: [::call(fname,arg1,...,argN)_html::],...

      A template may also include other templates with [::include(filename)::].
  *)


  (** [new template ?filename tpl] computes a new template from the
      string [tpl].  Once the object has been created, it can be used
      in a single thread.

      @param filename if set, it is used to determine the base path for
      [::include()::] tags in the template (default: current directory)
      and to reuse the templates of already compiled files
      (e.g. headers, footers,...).  *)
  class template : ?filename:string -> string ->
  object
    method set : string -> string -> unit
      (** Set a variable in the template. *)

    method table : string -> table_row list -> unit
      (** Set a table in the template. *)

    method conditional : string -> bool -> unit
      (** Set a conditional in the template. *)

    method callback : string -> (string list -> string) -> unit
      (** Set a callback in the template. *)

    method to_string : string
      (** Return the template as a string. *)

    method to_channel : out_channel -> unit
      (** Write the template to a channel. *)

    method output :(string -> unit) -> unit
      (** [output out] outputs the template, calling [out s] for each
	  write of a string [s]. *)

    method source : string
      (** Return the original source code for the template. *)
  end

  val template : string -> template
    (** Compile the template from a named file.  Not thread safe. *)

  val template_from_string : ?filename:string -> string -> template
    (** Compile the template from a literal string.  Not thread safe. *)

  val template_from_channel : ?filename:string -> in_channel -> template
    (** Compile the template from a channel.  Not thread safe. *)
end


module DbiPool :
sig
  module type DbiDriverT =
  sig
    type connection
    val connect : ?host:string -> ?port:string ->
      ?user:string -> ?password:string -> string -> connection
    val close : connection -> unit
    val closed : connection -> bool
    val commit : connection -> unit
    val ping : connection -> bool
    val rollback : connection -> unit
  end


  module DbiPool(Dbi_driver : DbiDriverT) :
    (sig
       type connection = Dbi_driver.connection
       val get : Cgi.Request.t ->
	 ?host:string -> ?port:string -> ?user:string -> ?password:string ->
	 string -> connection
     end)
    (** [module MyPool = DbiPool(Dbi_postgresql)] creates a pool of
	PostgreSQL database handles.  To use them:

	[let dbh = MyPool.get request "database_name"]

	Returns an unused or newly created [Dbi.connection] handle
	[dbh] from the pool of database handles which is valid until
	the end of the current request.

	The parameters uniquely identify the database name.  Separate
	pools are maintained for each combination of parameters.

	The connection is automatically returned to the pool at the end of
	the current request.  After this time the connection may be
	given away to another user.  For this reason, the calling code must
	NEVER stash the connection across requests (instead, call
	[get] to get a new handle each time).

	On returning the handle to the pool, the pool performs a
	ROLLBACK operation on the handle, thus losing any changes
	(INSERT, etc.) made to the database.  If the program wants to
	preserve changes, it must perform a COMMIT operation itself,
	by calling [Dbi.connection.commit].  *)
end

(** Dates conforming RFC 1123 *)
module Expires :
sig
  val make : int -> string
    (** [make s] generates an a date [s] seconds from now in fixed
	format (RFC 1123).  [s] may be negative.  The date format is
	suitable used for e.g. a HTTP 'Expires' header -- if [s < 0],
	it means the page expires in the past, and should be removed
	from content caches.  *)
  val past : unit -> string
    (** Generate an date in the past (in theory this forces caches
	along the way to remove content).  *)
  val short : unit -> string
    (** Generate a date now + 5 minutes.  This can typically be used
	for pages containing news which is updated frequently.  *)
  val medium : unit -> string
    (** Generate a date now + 24 hours.  This can be used for content
	generated from a database which doesn't change much.  *)
  val long : unit -> string
    (** Generate a date now + 2 years.  This should be used for
	content which really will never change.  *)
end

(** Elementary email functions *)
module Sendmail :
sig
  exception Failure of string
    (** This exception may be thrown by any of the functions in this
	module, to indicate some problem running sendmail.  *)

  val sendmail : string ref
    (** This contains the path to sendmail (or the binary which acts
	like sendmail, as in the case of exim).  Normally this is set
	to the correct value for your OS, eg. ["/usr/sbin/sendmail"].
    *)
  val sendmail_args : string ref
    (** This contains the arguments passed to sendmail, normally ["-t
	-i"].  You could add the ["-f"] option to specify the sender
	email address, provided that the current user is one of
	sendmail's trusted users.  *)

  val send : unit -> out_channel
    (** Begin sending an email.  This returns a channel on which you
	should write the mail headers, followed by a blank line,
	followed by the message body.  After writing the mail you
	must call {!CamlGI.Sendmail.close}.

	[send] does not perform any sort of processing or escaping on
	the message.  *)

  val close : out_channel -> unit
    (** Close the output channel.  You must call this on the channel
	returned from {!CamlGI.Sendmail.send}.  *)

  val send_mail : ?subject:string ->
    ?to_addr:string list -> ?cc:string list -> ?bcc:string list ->
    ?from:string -> ?content_type:string ->
    ?headers:(string * string) list ->
    string -> unit
    (** This is a less flexible, but simpler interface to sending
	mail.  You specify, optionally, Subject line, To, Cc, Bcc,
	From and Content-Type headers, and a body, and a mail is
	generated.

	[send_mail] will correctly escape headers, provided they
	are in strict 7 bit US-ASCII.  Its behaviour on non-7 bit
	sequences is currently undefined (and probably wrong).
	[send_mail] will not process or escape any part of the body.
    *)
end
