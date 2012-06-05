open OUnit
open Printf

let s = "string"
let i = 1
let i_s = sprintf "%d" i
let f = 2.1
let f_s = sprintf "%f" f

let test_batt_quote () =
  (* Interpolation in quotations *)
  assert_equal s <:batt<$s>>;
  assert_equal i_s <:batt<${i, %d}>>;
  assert_equal f_s <:batt<${f, %f}>>;
  ()

let suite =
  "xstrp4 tests" >::: [
    "test_batt_quote" >:: test_batt_quote;
  ]

let () =
  if not (was_successful (run_test_tt suite)) then
    exit 1
  else
    ()

