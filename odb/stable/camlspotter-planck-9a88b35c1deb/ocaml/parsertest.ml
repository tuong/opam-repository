open Spotlib.Spot
open Planck

(* This is required to call Syntaxerr.report_error correctly. *)
let _ = Location.input_name := ""

let rec parse_implementation stream = 
  let parse = new Plparser.rules in
  try
    match Token.Parser.run parse#implementation stream with
    | Result.Ok (v, _st') -> (Obj.magic v : Xparsetree.structure)
    | Result.Error (pos, s) -> 
        Format.eprintf "%a: syntax error: %s@." Position.Region.format pos s;
        raise Exit
  with
  | Syntaxerr.Error e -> 
      Format.eprintf "%a@." Syntaxerr.report_error e;
      raise Exit
  | Input.Parser.Critical_error (pos, s) ->
      Format.eprintf "%a : %s@." Position.File.format pos s;
      raise Exit

let rec parse_interface stream = 
  let parse = new Plparser.rules in
  try
    match Token.Parser.run parse#interface stream with
    | Result.Ok (v, _st') -> (Obj.magic v : Xparsetree.signature)
    | Result.Error (pos, s) -> 
        Format.eprintf "%a: syntax error: %s@." Position.Region.format pos s;
        raise Exit
  with
  | Syntaxerr.Error e -> 
      Format.eprintf "%a@." Syntaxerr.report_error e;
      raise Exit
  | Input.Parser.Critical_error (pos, s) ->
      Format.eprintf "%a : %s@." Position.File.format pos s;
      raise Exit

let parse_implementation_orig f =
  let ic = open_in f in
  let lexbuf = Lexing.from_channel ic in
  Location.init lexbuf f;
  let res = Parse.implementation lexbuf in
  close_in ic;
  (Obj.magic res : Xparsetree.structure)

let parse_interface_orig f =
  let ic = open_in f in
  let lexbuf = Lexing.from_channel ic in
  Location.init lexbuf f;
  let res = Parse.interface lexbuf in
  close_in ic;
  (Obj.magic res : Xparsetree.signature)

let _ = 
  let time_sum_orig = ref 0.0 in
  let time_sum_planck = ref 0.0 in

  let print_times time_orig time_planck = 
    let open Unix in
    Format.eprintf "x%f (original %f, planck %f)@."
      (time_planck /. time_orig)
      time_orig time_planck
  in

  Arg.parse ["-debug", Arg.Set Plphelper.do_debug, "do debug" ] (fun path ->
    let ic = open_in path in
    let stream = Input.Stream.from_chan ~filename:path ic in
    let token_stream = Lex.ocaml_token_stream stream in

    Format.eprintf "%s@." path;
    if String.is_postfix path ~postfix:".ml" then begin
      let res, time_orig = with_time parse_implementation_orig path in
      let plres, time_planck = with_time parse_implementation token_stream in
      if res <> plres then begin
        Format.eprintf "MISMATCH %s@." path;
        if !Plphelper.do_debug then begin
          Format.eprintf "PLANCK: %a@." Sexplib.Sexp.pp_hum (Xparsetree.sexp_of_structure plres);
          Format.eprintf "ORIGIN: %a@." Sexplib.Sexp.pp_hum (Xparsetree.sexp_of_structure res);
        end;
        assert false
      end else begin 
        time_sum_orig := !time_sum_orig +. time_orig;
        time_sum_planck := !time_sum_planck +. time_planck;
        print_times time_orig time_planck;
        Format.eprintf "Lexer: %t@." Input.Parser.Profile.format;
        Format.eprintf "Parser: %t@." Token.Parser.Profile.format;
        Input.Parser.Profile.reset ();
        Token.Parser.Profile.reset ();
      end
    end else if String.is_postfix path ~postfix:".mli" then begin
        let res, time_orig = with_time parse_interface_orig path in
        let plres, time_planck = with_time parse_interface token_stream in
        if res <> plres then begin
          Format.eprintf "MISMATCH %s@." path;
          if !Plphelper.do_debug then begin
            Format.eprintf "PLANCK: %a@." Sexplib.Sexp.pp_hum (Xparsetree.sexp_of_signature plres);
            Format.eprintf "ORIGIN: %a@." Sexplib.Sexp.pp_hum (Xparsetree.sexp_of_signature res);
          end;
          assert false
        end else begin
          time_sum_orig := !time_sum_orig +. time_orig;
          time_sum_planck := !time_sum_planck +. time_planck;
          print_times time_orig time_planck;
          Format.eprintf "Lexer: %t@." Input.Parser.Profile.format;
          Format.eprintf "Parser: %t@." Token.Parser.Profile.format;
          Input.Parser.Profile.reset ();
          Token.Parser.Profile.reset ();
        end;
    end;
    close_in ic;
  ) "parsertest files";
  prerr_endline "ALL TEST ENDED";
  print_times !time_sum_orig !time_sum_planck;
  Input.Parser.Profile.recover_all ();
  Token.Parser.Profile.recover_all ();
  Format.eprintf "Lexer: %t@." Input.Parser.Profile.format;
  Format.eprintf "Parser: %t@." Token.Parser.Profile.format




