(***********************************************************************)
(*                                                                     *)
(*                          CamlP3l                                    *)
(*                                                                     *)
(* (C) 2004-2007                                                       *)
(*             Roberto Di Cosmo (dicosmo@dicosmo.org)                  *)
(*             Zheng Li (li@pps.jussieu.fr)                            *)
(*             Pierre Weis (Pierre.Weis@inria.fr)                      *)
(*             Francois Clement (Francois.Clement@inria.fr)            *)
(*                                                                     *)
(* Based on original Ocaml P3L System                                  *)
(* (C) 1997 by                                                         *)
(*             Roberto Di Cosmo (dicosmo@ens.fr)                       *)
(*             Marco Danelutto (marcod@di.unipi.it)                    *)
(*             Xavier Leroy  (Xavier.Leroy@inria.fr)                   *)
(*             Susanna Pelagatti (susanna@di.unipi.it)                 *)
(*                                                                     *)
(* This program is free software; you can redistribute it and/or       *)
(* modify it under the terms of the GNU Library General Public License *)
(* as published by the Free Software Foundation; either version 2      *)
(* of the License, or (at your option) any later version.              *)
(*                                                                     *)
(* This program is distributed in the hope that it will be useful,     *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(* GNU Library General Public License for more details.                *)
(*                                                                     *)
(***********************************************************************)

(* $Id: uversionning.ml,v 1.2 2007-07-13 12:53:27 fclement Exp $ *)

(** Handling versionning generically. *)

open Ucommand_option;;

type software_info = {
 software_name : string;
 software_banner_name : string;
 software_major_version : int;
 software_minor_version : int;
 software_patch_level : int;
 software_dev_level : int;
 software_dev_info : string;
 software_version_date : string;
 software_first_year : int;
};;

let make_patch_level si =
  let patch_level = si.software_patch_level in
  if patch_level = 0 then "" else Printf.sprintf ".%d" patch_level
;;

let make_short_version si =
  Printf.sprintf "%d.%d%s"
    si.software_major_version
    si.software_minor_version
    (make_patch_level si)
;;

let make_dev_info si =
  let dev_info = si.software_dev_info in
  if dev_info = "" then "" else Printf.sprintf ", %s" dev_info
;;

let make_long_version si =
  Printf.sprintf ("%s+dev%d (%s%s)")
    (make_short_version si)
    si.software_dev_level
    si.software_version_date
    (make_dev_info si)
;;

let make_banner si version =
  Printf.sprintf "The %s, version %s" si.software_banner_name version
;;

let make_print_version make_version si =
  let version = make_version si in
  let banner = make_banner si version in
  (fun () ->
    prerr_endline banner;
    exit 0);;

let make_print_short_version = make_print_version make_short_version;;
let make_print_long_version = make_print_version make_long_version;;

(* Adding command line options. *)
let short_option_name si =
  let sn = si.software_name in
  if String.length sn = 0 then '-' else sn.[0]
;;

let add_version_options si =
  Ucommand_option.add_all
    [
     {key = "--version";
      spec = Arg.Unit (make_print_long_version si);
      doc =
        Printf.sprintf
          " %s detailed version information."
          si.software_banner_name};
     
     {key = "-v";
      spec = Arg.Unit (make_print_short_version si);
      doc =
        Printf.sprintf
          " %s short version information."
          si.software_banner_name};
   ];;
    
let add_library_version_options si =
  Ucommand_option.add_all
    [
     {key = Printf.sprintf "--%s-version" si.software_name;
      spec = Arg.Unit (make_print_long_version si);
      doc =
        Printf.sprintf
          " %s detailed version information."
          si.software_banner_name};
     
     {key = Printf.sprintf "-%cv" (short_option_name si);
      spec = Arg.Unit (make_print_short_version si);
      doc = 
        Printf.sprintf
          " %s short version information."
          si.software_banner_name;};
   ];;
