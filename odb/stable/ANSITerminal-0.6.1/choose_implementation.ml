(* Script to choose the unix or windows implementation depending on
   the platform *)

open Printf

let copy_file source target =
  let fh0 = open_in source in
  let fh1 = open_out target in
  let buf = String.create 4096 in
  let len = ref 1 in
  while !len > 0 do
    len := input fh0 buf 0 4096;
    output fh1 buf 0 !len
  done;
  close_in fh0;
  close_out fh1


let choose_unix () =
  copy_file "ANSITerminal_unix.ml" "ANSITerminal.ml";
  copy_file "ANSITerminal_unix_stubs.c" "ANSITerminal_stubs.c"

let choose_win () =
  copy_file "ANSITerminal_win.ml" "ANSITerminal.ml";
  copy_file "ANSITerminal_win_stubs.c" "ANSITerminal_stubs.c"

let () =
  match Sys.os_type with
  | "Unix" | "Cygwin" -> choose_unix ()
  | "Win32" -> choose_win()
  | e -> eprintf "Unknown OS type %S.\n" e
