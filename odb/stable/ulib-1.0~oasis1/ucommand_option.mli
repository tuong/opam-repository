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

(* $Id: ucommand_option.mli,v 1.4 2011-01-04 19:15:22 qcarbonn Exp $ *)

(** The module to handle options in the command line of the program.
   
   Regular command options are the association of an option name to be used on
   the command line, an action to be executed when parsing the command line,
   and a documentation to be printed for help.
   
   User command options are meant to be parsed separately by the user.
   
   Alias command options allow to define synonyms to the option name.
   
   Report command options are used to collect and print information during the
   execution of the program (e.g. for the monitoring of iterative algorithms).
   
   Debug command options are used to trace the execution when a program does
   not behave properly. *)

(** {6 Regular command options} *)

type t = {
    key : Arg.key;
    spec : Arg.spec;
    doc : Arg.doc;
  };;
type command_option = t;;
(** The type of command options.
   For a command option [opt], [opt.key] is the option name (a string starting
   with a '-' character), [opt.spec] is the action, and [opt.doc] is the
   documentation (a string). *)

val add : t -> unit;;
(** [add opt] adds the command option [opt.key] to the command line option list
   with specification [opt.spec] and manual information [opt.doc]. *)

val add_all : t list -> unit;;
(** [add_all opts] adds all the command options of the list [opts]. *)

val all : unit -> t list;;
(** [all ()] returns all command options with preservation of the order. *)

val parse : Arg.anon_fun -> Arg.usage_msg -> unit;;
(** [parse af msg] parses the command line to treat command options. *)

val parse_argv : string array -> Arg.anon_fun -> Arg.usage_msg -> unit;;
(** [parse_argv l af msg] parses the string array [l] to treat command
   options. *)

val usage : Arg.usage_msg -> unit
(** [usage msg] prints an error message including the list of valid
   options. This is the same message as {!parse} prints in case of
   error. [msg] is the same as for [parse] *)

(** {6 Advanced command options} *)

type plain_command_option =
  | Regular of t
  | User of t * Arg.spec
  | Alias of plain_command_option * Arg.key
;;
(** The type of plain command options.
   
   [User (opt, spc)] is the regular command option [opt] to parse it with the
   [parse_user] function below. The extra [spc] is used to create the regular
   command option [\{opt with spec = spc\}] to parse it with [Arg.parse].
   
   [Alias (opt, k)] is the plain command option [opt] with [k] as a synonym to
   the option name. *)

type advanced_command_option =
  | Plain of plain_command_option
  | Report of plain_command_option
  | Debug of plain_command_option
;;
(** The type of advanced command options.
   
   [Report opt] is a plain command option that can also be fired by using [-r]
   and [--report] built-in option names.
   
   [Debug opt] is a plain command option that can also be fired by using [-d]
   and [--debug] built-in option names. *)
   

val add_advanced : advanced_command_option -> unit;;
(** [add_advanced opt] adds the advanced command option [opt] to the command
   line option list. *)

val add_advanced_all : advanced_command_option list -> unit;;
(** [add_advanced_all opts] adds all the advanced command options of the list
   [opts]. *)

val add_bi_all : (t * Arg.key) list -> unit;;
(** [add_bi_all l] adds all items [opt, k] of the list [l] as the advanced
   command option [Plain (Alias (Regular opt, k))]. *)

val add_report_bi_all :  (t * Arg.key) list -> unit;;
(** [add_report_bi_all l] adds all items [opt, k] of the list [l] as the
   advanced command option [Report (Alias (Regular opt, k))]. *)

val add_debug_bi_all :  (t * Arg.key) list -> unit;;
(** [add_debug_bi_all l] adds all items [opt, k] of the list [l] as the advanced
   command option [Debug (Alias (Regular opt, k))]. *)

val add_user_bi_all :  ((t * Arg.spec) * Arg.key) list -> unit;;
(** [add_user_bi_all l] adds all items [(opt, s), k] of the list [l] as the
   advanced command option [Plain (Alias (User (opt, s)), k)]. *)

val add_user_report_bi_all :  ((t * Arg.spec) * Arg.key) list -> unit;;
(** [add_report_bi_all l] adds all items [(opt, s), k] of the list [l] as the
   advanced command option [Report (Alias (User (opt, s), k))]. *)

val add_user_debug_bi_all :  ((t * Arg.spec) * Arg.key) list -> unit;;
(** [add_debug_bi_all l] adds all items [(opt, s), k] of the list [l] as the
   advanced command option [Debug (Alias (User (opt, s), k))]. *)

val parse_user : unit -> unit;;
(** [parse_user ()] parses the command line to treat user command options (and
   only user command options). *)

val flag : bool -> Arg.key -> Arg.doc -> bool ref;;
(** [flag init key doc] adds the option flag [key] to the command line option
   list with manual information [doc] and returns the corresponding boolean
   reference flag. This reference flag has initial boolean value [init], and is
   modified with the command line option [key]: if the initial value [init] is
   [false] then the option [key] sets the flag; otherwise the option [key]
   clears the flag. *)
