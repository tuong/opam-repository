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

module Sexp = Sexplib.Sexp
module Conv = Sexplib.Conv
open Sexplib.Wrapper
open Bin_prot.Std

let sexp_of_exn = Conv.sexp_of_exn
let sexp_of_exn_opt = Conv.sexp_of_exn_opt

type t = exn with sexp_of

exception Finally of t * t with sexp
exception Reraised of string * t with sexp

let reraise exc str =
  raise (Reraised (str, exc))

let reraisef exc format =
  Printf.ksprintf (fun str () -> reraise exc str) format

let () =
  StdLabels.List.iter
    ~f:(fun (exc, handler) ->
      Conv.Exn_converter.add_auto ~finalise:false exc handler)
    [
      (
        Bin_prot.Common.Read_exc (Not_found, 0),
        (function
        | Bin_prot.Common.Read_exc (exc, pos) ->
            Sexp.List [
              Sexp.Atom "Bin_prot.Common.Read_exc";
              sexp_of_exn exc;
              Conv.sexp_of_int pos;
            ]
        | _ -> assert false)
      );(
        Bin_prot.Common.Read_error (Bin_prot.Common.ReadError.Neg_int8, 0),
        (function
        | Bin_prot.Common.Read_error (err, pos) ->
            let str_err = Bin_prot.Common.ReadError.to_string err in
            Sexp.List [
              Sexp.Atom "Bin_prot.Common.Read_error";
              Sexp.Atom str_err;
              Conv.sexp_of_int pos;
            ]
        | _ -> assert false)
      );(
        Bin_prot.Unsafe_read_c.Error Bin_prot.Common.ReadError.Neg_int8,
        (function
        | Bin_prot.Unsafe_read_c.Error err ->
            let str_err = Bin_prot.Common.ReadError.to_string err in
            Sexp.List [ Sexp.Atom "Bin_prot.Common.Read_error";
                              Sexp.Atom str_err ]
        | _ -> assert false)
      )
    ]

let to_string exc = Sexp.to_string_hum ~indent:2 (sexp_of_exn exc)

let sexp_of_t = sexp_of_exn

let protectx ~f x ~(finally : _ -> unit) =
  let res =
    try f x
    with exn ->
      (try finally x with final_exn -> raise (Finally (exn, final_exn)));
      raise exn
  in
  finally x;
  res

let protect ~f ~finally = protectx ~f () ~finally

let pp ppf t = Sexp.pp_hum ppf (sexp_of_exn t)

let backtrace = Printexc.get_backtrace

let catch_and_print_backtrace ~exit f =
  try f ()
  with exc ->
    let bt = backtrace () in
    Format.eprintf "@[<2>Uncaught exception:@\n@\n@[%a@]@]@." pp exc;
    if Printexc.backtrace_status () then prerr_string bt;
    exit 1

let handle_uncaught ~exit:must_exit f =
  catch_and_print_backtrace f ~exit:(
    if must_exit
    then exit
    else ignore
  )

let reraise_uncaught str func =
  try func () with
  | exn -> raise (Reraised (str, exn))

let () = Pretty_printer.register "Core.Exn.pp"

let () =
  Printexc.register_printer (fun exc ->
    Option.map (sexp_of_exn_opt exc) ~f:(fun sexp ->
      Sexp.to_string_hum ~indent:2 sexp))
