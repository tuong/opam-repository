open Spotlib.Spot
open Planck

let rec parse_and_print stream = 
  match Token.Stream.peek stream with
  | None -> ()
  | Some (elem, (_,pos,_), stream') ->
      Format.eprintf "%s @@ %a@."
        (Sexplib.Sexp.to_string_hum (Token.sexp_of_t elem))
        Position.Region.format pos;
      parse_and_print stream'

let _ = Arg.parse [] (fun x ->
  let ic = open_in x in
  let stream = Input.Stream.from_chan ~filename:"" ic in
  let token_stream = Lex.ocaml_token_stream stream in
  parse_and_print token_stream;
  close_in ic;
) "ocamlyacctest files"

