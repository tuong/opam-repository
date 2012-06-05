open OUnit
open Printf

let s = "string"
let i = 1
let i_s = sprintf "%d" i
let f = 2.1
let f_s = sprintf "%f" f

let test_strings () =
  (* Explicit interpolations *)
  assert_equal s "$s";
  assert_equal i_s "${i, %d}";
  assert_equal f_s "${f, %f}";
  ()

let suite =
  "xstrp4 tests" >::: [
    "test_strings" >:: test_strings;
  ]

let () =
  if not (was_successful (run_test_tt suite)) then
    exit 1
  else
    ()

