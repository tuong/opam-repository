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

(* $Id: ucommand_option.ml,v 1.7 2011-01-04 19:15:22 qcarbonn Exp $ *)

(* Regular command options. *)

type t = {
  key : Arg.key;
  spec : Arg.spec;
  doc : Arg.doc;
}
;;

type command_option = t;;

let triple_of_command_option { key = k; spec = s; doc = d } = k, s, d;;

let regular_options = ref [];;

let add t = regular_options := t :: !regular_options;;

let add_all = List.iter add;;

let all () = List.rev !regular_options;;

let parse af msg =
  let opts = List.rev !regular_options in
  Arg.parse (List.map triple_of_command_option opts) af msg
;;

let parse_argv argv af msg =
  let opts = List.rev !regular_options in
  Arg.parse_argv argv (List.map triple_of_command_option opts) af msg
;;

let usage msg =
  let opts = List.rev !regular_options in
  Arg.usage (List.map triple_of_command_option opts) msg
;;

(* Advanced command options. *)

type plain_command_option =
  | Regular of t
  | User of t * Arg.spec
  | Alias of plain_command_option * Arg.key
;;

type advanced_command_option =
  | Plain of plain_command_option
  | Report of plain_command_option
  | Debug of plain_command_option
;;

let user_options = ref [];;

let add_user (t, s) =
  user_options := t :: !user_options;
  add { t with spec = s; }
;;

let is_user_option k = List.exists (fun opt -> opt.key = k) !user_options;;

let report_specs = ref []
and debug_specs = ref []
;;

let launch_report_specs () = List.iter (fun spec -> spec ()) !report_specs
and launch_debug_specs () = List.iter (fun spec -> spec ()) !debug_specs
;;

let add_unit_spec f = function
  | Arg.Unit spec -> f spec
  | Arg.Bool _
  | Arg.Set _
  | Arg.Clear _
  | Arg.String _
  | Arg.Set_string _
  | Arg.Int _
  | Arg.Set_int _
  | Arg.Float _
  | Arg.Set_float _
  | Arg.Tuple _
  | Arg.Symbol _
  | Arg.Rest _ -> invalid_arg "add_unit_spec: not a unit argument"
;;

let add_report_spec =
  add_unit_spec (fun spec -> report_specs := spec :: !report_specs)
and add_debug_spec =
  add_unit_spec (fun spec -> debug_specs := spec :: !debug_specs)
;;

let add_report t = add t; add_report_spec t.spec
and add_debug t = add t; add_debug_spec t.spec;;

let add_user_report (t, _ as opt) =
  add_user opt; add_report t
and add_user_debug (t, _ as opt) =
  add_user opt; add_debug t;;

let alias_doc k = Printf.sprintf "  Alias for %s.\n" k;;

let rec alias_opt popt k =
  match popt with
  | Regular t -> Regular {t with key = k; doc = alias_doc t.key}
  | User (t, s) -> User ({t with key = k; doc = alias_doc t.key}, s)
  | Alias (popt, k) -> alias_opt popt k
;;

let rec add_advanced = function
  | Plain (Regular t) -> add t
  | Report (Regular t) -> add_report t
  | Debug (Regular t) -> add_debug t
  | Plain (User (t, s)) -> add_user (t, s)
  | Report (User (t, s)) -> add_user_report (t, s)
  | Debug (User (t, s)) -> add_user_debug (t, s)
  | Plain (Alias (popt, k)) ->
      add_advanced (Plain popt);
      add_advanced (Plain (alias_opt popt k))
  | Report (Alias (popt, k)) ->
      add_advanced (Report popt);
      add_advanced (Report (alias_opt popt k))
  | Debug (Alias (popt, k)) ->
      add_advanced (Debug popt);
      add_advanced (Debug (alias_opt popt k))
;;

let add_advanced_all = List.iter add_advanced;;

let add_bi (popt, k) = add_advanced (Plain (Alias (Regular popt, k)));;

let add_bi_all = List.iter add_bi;;

let add_report_bi (popt, k) = add_advanced (Report (Alias (Regular popt, k)))
and add_debug_bi (popt, k) = add_advanced (Debug (Alias (Regular popt, k)))
;;

let add_report_bi_all = List.iter add_report_bi
and add_debug_bi_all = List.iter add_debug_bi
;;

let add_user_bi ((t, s), k) = add_advanced (Plain (Alias (User (t, s), k)));;

let add_user_bi_all = List.iter add_user_bi;;

let add_user_report_bi ((t, s), k) =
  add_advanced (Report (Alias (User (t, s), k)))
and add_user_debug_bi ((t, s), k) =
  add_advanced (Debug (Alias (User (t, s), k)))
;;

let add_user_report_bi_all = List.iter add_user_report_bi
and add_user_debug_bi_all = List.iter add_user_debug_bi
;;

add_bi_all [
  {key = "--report";
   spec = Arg.Unit launch_report_specs;
   doc = "  Turn on all declared report command options. Default is off."},
  "-r";

  {key = "--debug";
   spec = Arg.Unit launch_debug_specs;
   doc = "  Turn on all declared debug command options. Default is off."},
  "-d";
]
;;

let rec assoc3 x l =
  match l with
  | [] -> raise Not_found
  | (y1, y2, _y3) :: _t when y1 = x -> y2
  | _ :: t -> assoc3 x t
;;

let is_option s = String.length s >= 1 && String.get s 0 = '-';;

let parse_user () =
  let all_options = List.map triple_of_command_option !regular_options in
  let user_options = List.map triple_of_command_option !user_options in
  (* Creating an argv with only user command option relevant arguments. *)
  let argv =
    let args = Sys.argv in
    let current = ref 0
    and l = Array.length args in
    let arg_list = ref [args.(0)] in
    let rec loop () =
      if !current < l then
        begin
          let s = args.(!current) in
          if is_option s then
            let for_user = is_user_option s in
            let add_to_arg_list s =
              incr current;
              if for_user then arg_list := s :: !arg_list in
            add_to_arg_list s;
            let action =
              try assoc3 s all_options with
              | Not_found ->
                  if not (s = "-h" || s = "-help" || s = "--help") then
                    begin
                      prerr_endline (Printf.sprintf "Unknown option %s" s);
                      exit 2
                    end else Arg.Unit (fun () -> ()) in
            let rec treat_action = function
            | Arg.Unit _
            | Arg.Set _
            | Arg.Clear _
            | Arg.Set_string _
            | Arg.Set_int _
            | Arg.Set_float _
            | Arg.Rest _ -> ()
            | Arg.Tuple specs -> List.iter treat_action specs
            | Arg.Bool _
            | Arg.String _
            | Arg.Int _
            | Arg.Float _
            | Arg.Symbol (_, _)
              -> add_to_arg_list args.(!current) in
            treat_action action
          else incr current;
          loop ()
        end in
    loop ();
    Array.of_list (List.rev !arg_list) in
  let current = ref 0 in
  Arg.parse_argv ~current: current argv user_options ignore ""
;;

(* A special case: flag options *)
let flag init option_name message =
  let r = ref init in
  let opt =
    {
      key = option_name;
      spec = (if init then Arg.Clear r else Arg.Set r);
      doc = message;
    } in
  add opt;
  r
;;
