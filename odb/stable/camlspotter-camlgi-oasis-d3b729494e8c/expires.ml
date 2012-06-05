(* File: expires.ml

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
(* 	$Id: expires.ml,v 1.1 2005/01/19 14:14:12 chris_77 Exp $	 *)

open Cgi_common

(* RFC 1123 fixed date format *)
let make offset =
  let t = Unix.time () in
  let tm = Unix.gmtime (t +. float offset) in
  Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT"
    (string_of_weekday tm.Unix.tm_wday) tm.Unix.tm_mday
    (string_of_month tm.Unix.tm_mon)    (tm.Unix.tm_year + 1900)
    tm.Unix.tm_hour  tm.Unix.tm_min  tm.Unix.tm_sec

let past () = make (-300) (* -5 * 60 *)
let short () = make 300   (* 5 * 60 *)
let medium () = make 86400
let long () = make 63072000 (* 365 * 2 * 86400 *)
