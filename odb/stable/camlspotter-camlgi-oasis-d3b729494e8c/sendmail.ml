(* File: sendmail.ml

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
(* 	$Id: sendmail.ml,v 1.1 2005/01/19 14:14:12 chris_77 Exp $	 *)

open Printf

exception Failure of string

let sendmail = ref "/usr/sbin/sendmail"
let sendmail_args = ref "-t -i"

let send () =
  Unix.open_process_out(!sendmail ^ " " ^ !sendmail_args)

let close chan =
  match Unix.close_process_out chan with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n ->
      raise(Failure(sprintf "%s: non-zero exit status: %i" !sendmail n))
  | Unix.WSIGNALED n ->
      raise(Failure(sprintf "%s: killed by signal %i" !sendmail n))
  | Unix.WSTOPPED n ->
      raise(Failure(sprintf "%s: stopped by signal %i" !sendmail n))


let nl_folding s =
  let n_nl = ref(String.length s) in
  for i = 0 to String.length s - 1 do
    if String.unsafe_get s i = '\n' then incr n_nl
  done;
  let t = String.create !n_nl in
  let rec folding i j =
    let c = s.[i] in
    t.[j] <- c;
    if c = '\n' then (t.[j+1] <- '\t'; folding (i + 1) (j + 2))
    else folding (i + 1) (j + 1) in
  folding 0 0

let output_header chan k v =
  output_string chan k;
  output_string chan ": ";
  output_string chan (nl_folding v);
  output_char chan '\n'

let output_header_opt1 chan k = function
  | None -> ()
  | Some v -> output_header chan k v

let output_header_optN chan k = function
  | None -> ()
  | Some v -> output_header chan k (String.concat ", " v)

let send_mail ?subject ?to_addr ?cc ?bcc ?from ?content_type ?headers body =
  let chan = send () in
  (match headers with
   | None -> ()
   | Some hs -> List.iter (fun (k,v) -> output_header chan k v) hs);
  output_header_opt1 chan "Subject" subject;
  output_header_optN chan "To" to_addr;
  output_header_optN chan "Cc" cc;
  output_header_optN chan "Bcc" bcc;
  output_header_opt1 chan "From" from;
  output_header_opt1 chan "Content-Type" content_type;
  output_char chan '\n'; (* blank line = end of headera. *)
  (* Send the body. *)
  output_string chan body;
  (* Close connection. *)
  close chan
