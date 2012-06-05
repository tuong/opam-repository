open Camlp4.PreCast
open Syntax

(** Use the default implementation as-is *)
module M = Xstrp4.Make(Xstrp4.Default_implementation)(Xstrp4.Default_lexer)

(** Interpolating file contents *)
let interpolated_file filename _loc =
  let pathname =
    if Filename.is_implicit filename then
      Filename.concat (Filename.dirname (Loc.file_name _loc)) filename
    else
      filename
  in
  let f = open_in pathname in
  let lexbuf = Lexing.from_channel f in
  let _loc = Loc.of_tuple (pathname, 1, 0, 0, 1, 0, 0, false) in
  M.interpolated_expr lexbuf _loc

(** Including file contents *)
let included_file filename _loc =
  let pathname =
    if Filename.is_implicit filename then
      Filename.concat (Filename.dirname (Loc.file_name _loc)) filename
    else
      filename
  in
  let f = open_in pathname in
  let n = in_channel_length f in
  let s = String.create n in
  really_input f s 0 n;
  close_in f;
  <:expr< $str:s$ >>

let interpolation = Gram.Entry.mk "interpolation"

EXTEND Gram
  interpolation:
    [[ s = STRING ->
         let lexbuf = Lexing.from_string s in
         M.interpolated_expr lexbuf _loc
     ]];

  expr: AFTER "simple"
    [[ "interpolate_file"; s = STRING -> interpolated_file s _loc
     | "interpolate"; expr = interpolation -> expr
     | "include_file"; s = STRING -> included_file s _loc
     ]];

END

