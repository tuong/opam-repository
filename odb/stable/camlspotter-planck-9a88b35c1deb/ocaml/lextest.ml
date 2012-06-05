open Planck

let rec parse_and_print stream = 
  match Input.Parser.run (Input.Parser.eos_as_none Lex.ocaml_token) stream with
  | Result.Ok (None, _) -> () (* EOS *)
  | Result.Ok (Some (v, pos), stream') ->
      let show t = Sexplib.Sexp.to_string_hum (Token.sexp_of_t t) in
      Format.eprintf "%s[%a]@." 
        (show v) 
        Position.Region.format pos;
      parse_and_print stream'
  | Result.Error (pos, s) -> 
      Format.eprintf "%a: syntax error: %s@." Position.File.format pos s

let _ = Arg.parse [] (fun x ->
  let ic = open_in x in
  let stream = Input.Stream.from_chan ~filename:"" ic in
  parse_and_print stream;
  close_in ic) "lextest files"

