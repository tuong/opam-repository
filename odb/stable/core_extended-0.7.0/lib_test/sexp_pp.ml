(******************************************************************************
 *                             Core-extended                                  *
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
open Core_extended.Std
let engine : [`Alter | `New | `Old] ref = ref `New

let alter sexp =
  Format.pp_set_margin Format.std_formatter 80;
  Sexp.pp_hum'  Format.std_formatter sexp;
  Format.pp_print_newline Format.std_formatter ()

let pp sexp =
  match !engine with
  | `New -> Pp.to_file stdout (Sexp.format sexp)
  | `Old -> Sexp.output_hum stdout sexp
  | `Alter -> alter sexp

let spec =
  [ "-old",Arg.Unit (fun () -> engine := `Old)," Pretty print with sexp's code";
    "-alter",Arg.Unit (fun () -> engine := `Alter)," Pretty print with sexp's code"
  ]

let usage = sprintf "%s [flags] [file]..."
  (Filename.basename Sys.executable_name)

let main () =
  let is_piped = not (Unix.isatty Unix.stdin) in
  let args = ref [] in
  Arg.parse spec (fun s -> args:= s:: !args) usage ;
  match List.rev !args with
  | [] ->
      if is_piped then begin
        List.iter ~f:pp (Sexp.input_sexps stdin)
      end else begin
        Arg.usage spec usage;
        exit 1
      end
  | l ->
      List.concat_map ~f:Sexp.load_sexps l
      |! List.iter ~f:pp


let () = Exn.handle_uncaught ~exit:true main
