open Spotlib.Spot
open Planck

(* This is required to call Syntaxerr.report_error correctly. *)
let _ = Location.input_name := ""

let rec parse stream = 
  match Token.Stream.peek stream with
  | Some (_, _, stream') -> parse stream'
  | None -> ()

let parse_orig f =
  let ic = open_in f in
  let lexbuf = Lexing.from_channel ic in
  Location.init lexbuf f;
  let rec loop () = 
    match Lexer.token lexbuf with
    | Parser.EOF -> ()
    | _ -> loop ()
  in
  loop ();
  close_in ic

let _ = 
  let time_sum_orig = ref 0.0 in
  let time_sum_planck = ref 0.0 in

  let print_times time_orig time_planck = 
    let open Unix in
    Format.eprintf "x%f (original %f, planck %f)@."
      (time_planck /. time_orig)
      time_orig time_planck
  in

  Arg.parse [] (fun path ->
    let ic = open_in path in
    let stream = Input.Stream.from_chan ~filename:path ic in
    let token_stream = Lex.ocaml_token_stream stream in

    Format.eprintf "%s@." path;

    let (), time_orig = with_time parse_orig path in
    let (), time_planck = with_time parse token_stream in
    time_sum_orig := !time_sum_orig +. time_orig;
    time_sum_planck := !time_sum_planck +. time_planck;
    print_times time_orig time_planck;
    Format.eprintf "Lexer: %t@." Input.Parser.Profile.format;
    Input.Parser.Profile.reset ();

    close_in ic;
  ) "lexertest files";
  prerr_endline "ALL TEST ENDED";
  print_times !time_sum_orig !time_sum_planck;
  Input.Parser.Profile.recover_all ();
  Format.eprintf "Lexer: %t@." Input.Parser.Profile.format




