open OUnit
open Printf

let s = "string"
let i = 1
let i_s = sprintf "%d" i
let f = 2.1
let f_s = sprintf "%f" f

let test_here () =
  (* Explicit interpolations *)
  assert_equal s (interpolate "$s");
  assert_equal i_s (interpolate "${i, %d}");
  assert_equal f_s (interpolate "${f, %f}");
  ()

let test_here_quote () =
  (* Interpolation in quotations *)
  assert_equal s <:here<$s>>;
  assert_equal i_s <:here<${i, %d}>>;
  assert_equal f_s <:here<${f, %f}>>;
  ()

let suite =
  "xstrp4 tests" >::: [
    "test_here" >:: test_here;
    "test_here_quote" >:: test_here_quote;
  ]

let () =
  if not (was_successful (run_test_tt suite)) then
    exit 1
  else
    ()

