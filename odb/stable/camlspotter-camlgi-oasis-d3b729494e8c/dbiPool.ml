(* File: cgi.ml

   Objective Caml Library for writing (F)CGI programs.

   Copyright (C) 2004

     Christophe Troestler
     email: Christophe.Troestler@umh.ac.be
     WWW: http://www.umh.ac.be/math/an/software/

   Copyright (C) 2003 Merjis Ltd.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation, with the
   special exception on linking described in file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*)
(* 	$Id: dbiPool.ml,v 1.1 2005/06/12 21:43:02 chris_77 Exp $	 *)

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


module DbiPool (Dbi_driver : DbiDriverT) = struct
  type connection = Dbi_driver.connection

  (* List of pools. The key is the unique combination of host/port/etc. and
   * the value is a list of unused connections for that pool.
   *
   * This code ought to work even for a multi-threaded Apache server.
   *)
  let pools = Hashtbl.create 8

  let key ?host ?port ?user ?password database_name =
    host, port, user, password, database_name

  let get r ?host ?port ?user ?password database_name =
    let key = key ?host ?port ?user ?password database_name in

    (* Get the pool (a connection list). *)
    let dbh_list = try Hashtbl.find pools key with Not_found -> [] in

    (* Search for an unused connection. We actually iterate over the
     * pool testing the handles (in case they have timed out or
     * something).  *)
    let rec loop = function
	[] ->
	  (* No handles left. Need to create a new connection. *)
	  let dbh =
	    Dbi_driver.connect ?host ?port ?user ?password database_name in
	  dbh, []
      | dbh :: dbhs ->
	  (* Test if dbh is a working handle. If so, return it. *)
	  if Dbi_driver.ping dbh then
	    dbh, dbhs
	  else (
	    Dbi_driver.close dbh;
	    loop dbhs
	  )
    in
    let dbh, remainder = loop dbh_list in

    (* Update the pool. *)
    Hashtbl.replace pools key remainder;

    (* Register a callback so that we return this handle to the pool
     * when the request finishes.
     *)
(*     Request.register_cleanup r *)
(*       (fun () -> *)
	 if not (Dbi_driver.closed dbh) then (
	   Dbi_driver.rollback dbh;
	   let dbh_list = Hashtbl.find pools key in
	   Hashtbl.replace pools key (dbh_list @ [dbh])
	 );
(* ); *)

    dbh

end
