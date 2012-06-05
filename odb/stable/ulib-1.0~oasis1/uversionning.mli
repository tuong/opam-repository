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

(* $Id: uversionning.mli,v 1.1 2007-04-02 17:48:29 weis Exp $ *)

(* Handling versioning of the software. *)

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

val make_short_version : software_info -> string;;
val make_long_version : software_info -> string;;
 (** Given a [software_info] record, computes the current version names of the
  software.
  [make_short_version] computes the short version name with format
  ["major.minor"].
  [make_long_version] computes the long version name with format
  ["major.minor.patch_level+dev_level (version_date[, dev_info])"].
  Fields [major], [minor], and [patch_level] are integers,
  [additional_info] is an arbitrary string, and version_date presumably
  contains the date of last modification of the source tree. *)

val make_print_short_version : software_info -> unit -> unit;;
 (** Given a [software_info] record, returns a function that
   prints the current short version of the software on [stderr].
   The intended use of this function is to compute a function to handle the
   [-v] command line option parsed with the [Arg] standard library module. *)

val make_print_long_version : software_info -> unit -> unit;;
 (** Same as above for the long version of the software.
   The intended use of this function is to compute a function to handle the
   [--version] command line option parsed with the [Arg] standard library
   module. *)

val add_version_options : software_info -> unit;;
 (** Compute the version information for [software_info] and add relevant
   [-v] and [--version] command options using the module [Command_options]. *)

val add_library_version_options : software_info -> unit;;
 (** Compute the version information for [software_info] and add relevant
   [-*v] and [--*-version] command options using the module [Command_option]
   (where the [*] stand for some distinctive string computed from the name of
   the library). *)
