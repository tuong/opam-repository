(******************************************************************************
 *                             Core                                           *
 *                                                                            *
 * Copyright (C) 2008- Jane Street Holding, LLC                               *
 *    Contact: opensource@janestreet.com                                      *
 *    WWW: http://www.janestreet.com/ocaml                                    *
 *                                                                            *
 *                                                                            *
 * This library is free software; you can redistribute it and/or              *
 * modify it under the terms of the GNU Lesser General Public                 *
 * License as published by the Free Software Foundation; either               *
 * version 2 of the License, or (at your option) any later version.           *
 *                                                                            *
 * This library is distributed in the hope that it will be useful,            *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 * Lesser General Public License for more details.                            *
 *                                                                            *
 * You should have received a copy of the GNU Lesser General Public           *
 * License along with this library; if not, write to the Free Software        *
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                            *
 ******************************************************************************)

open Core.Std

type z = (string, int) Hashtbl.t with sexp

exception Bad of string with sexp

let match_char s =
  if String.length s <> 1 then
    raise (Bad s)
  else
    match s.[0] with
    | '0' -> `Heartbeat
    | '1' -> `Test_request
    | '2' -> `Resend_request
    | '3' -> `Reject_session
    | '4' -> `Sequence_reset
    | '5' -> `Logout
    | '6' -> `Indication_of_interest
    | '7' -> `Advertisement
    | '8' -> `Execution
    | '9' -> `Reject_cancel
    | 'A' -> `Logon
    | 'B' -> `News
    | 'C' -> `Email
    | 'D' -> `Order
    | 'F' -> `Cancel
    | 'G' -> `Cancel_replace
    | 'j' -> `Business_reject
    | _ -> raise (Bad s)
;;

let match_string s =
  match s with
  | "0" -> `Heartbeat
  | "1" -> `Test_request
  | "2" -> `Resend_request
  | "3" -> `Reject_session
  | "4" -> `Sequence_reset
  | "5" -> `Logout
  | "6" -> `Indication_of_interest
  | "7" -> `Advertisement
  | "8" -> `Execution
  | "9" -> `Reject_cancel
  | "A" -> `Logon
  | "B" -> `News
  | "C" -> `Email
  | "D" -> `Order
  | "F" -> `Cancel
  | "G" -> `Cancel_replace
  | "j" -> `Business_reject
  | _ -> raise (Bad s)
;;

let () =
  let before = Time.now () in
  let num_iter = 100_000_000 in
  for i = 1 to num_iter; do
    ignore (match_string "j");
  done;
  let after = Time.now () in
  let diff = Time.diff after before in
  let diff = (Time.Span.to_float diff /. float num_iter) *. 1E9 in
  printf "%.0f\n" diff;
;;
