(***********************************************************************)
(*                                                                     *)
(*                              My_stdlib                              *)
(*                                                                     *)
(*             projets Estime & Cristal, INRIA-Rocquencourt            *)
(*                                                                     *)
(*  Copyright 2004-2007 INRIA.                                         *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved. This file is distributed only by permission.  *)
(*                                                                     *)
(*  Francois Clement <Francois.Clement@inria.fr>                       *)
(*  Pierre Weis <Pierre.Weis@inria.fr>                                 *)
(*                                                                     *)
(***********************************************************************)

(* $Id: utest.ml,v 1.2 2008-04-24 10:46:25 weis Exp $ *)

(* A simple module to automate function testing. *)

(* Local auxiliaries *)
let module_name = ref "";;

let all_tests_ok = ref true;;

(* Test numbering *)
let initial_test_number = -1;;

let local_test_num, global_test_num =
  ref initial_test_number, ref initial_test_number
;;

let testing modname =
  module_name := modname;
  prerr_endline ("\nTesting " ^ modname);
  local_test_num := initial_test_number;
;;

(* Finishing *)
let test_suit_failed = "\n\n********** Test suit failed **********\n"
;;

let fail () = failwith test_suit_failed 
;;

let finish () =
  testing "results:";
  let print_tests_performed () =
    prerr_endline
      (Printf.sprintf "Total: %i tests performed." (1 + !global_test_num)) in
  print_tests_performed ();
  match !all_tests_ok with
  | true -> prerr_endline "\n\nAll tests succeeded.\n"
  | _ -> prerr_endline test_suit_failed
;;

at_exit finish;;

(* Printing test status. *)
let print_test_number () =
  print_int !local_test_num; print_string " "; flush stdout;
;;

let next_test () =
 incr global_test_num;
 incr local_test_num;
 print_test_number ();
;;

let print_test_fail () =
  all_tests_ok := false;
  prerr_endline
   (Printf.sprintf
      "\n\n********* %s: Test number %i failed ***********"
      !module_name !local_test_num);
  fail ();
;;

let print_failure_test_fail () =
  all_tests_ok := false;
  prerr_endline
   (Printf.sprintf
      "\n\n********* %s: Failure Test number %i incorrectly failed ***********"
      !module_name !local_test_num);
  fail ();
;;

let print_failure_test_succeed () =
  all_tests_ok := false;
  prerr_endline
   (Printf.sprintf
      "\n\n********* %s: Failure Test number %i failed to fail ***********"
      !module_name !local_test_num);
  fail ();
;;

let print_test_number_failed i =
  all_tests_ok := false;
  next_test ();
  prerr_endline
   (Printf.sprintf
      "\n\n********* %s: Test number %i is declared to be %i ***********"
      !module_name (!local_test_num + 1) i);
  fail ();
;;

let print_test_label_non_increasing i =
  all_tests_ok := false;
  next_test ();
  prerr_endline
   (Printf.sprintf
      "\n\n********* %s: Test label %i is not greater than last test label %i ***********"
      !module_name i !local_test_num);
  fail ();
;;

(* The testing functions. *)
let test b =
 next_test ();
 if not b then print_test_fail ()
;;

let testi i b =
 if i = !local_test_num + 1 then test b else print_test_number_failed i
;;

let testl i b =
 if i >= !local_test_num then
   begin
    local_test_num := i;
    test b
   end
 else print_test_label_non_increasing i
;;

(* Applies f to x and checks that the evaluation indeed
   raises an exception that verifies the predicate [pred]. *)
let test_raises_exc_p pred f x =
 next_test ();
 try
   ignore (f x);
   print_failure_test_succeed ()
 with
 | x ->
   pred x || print_failure_test_fail ()
;;

(* Applies f to x and checks that the evaluation indeed
   raises some exception. *)
let test_raises_some_exc f = test_raises_exc_p (fun _ -> true) f
and test_raises_this_exc exc = test_raises_exc_p (fun x -> x = exc)
;;

(* Applies f to x and checks that the evaluation indeed
   raises exception Failure s. *)

let test_raises_this_failure s f x =
  test_raises_exc_p (fun x -> x = Failure s) f x
;;

(* Applies f to x and checks that the evaluation indeed
   raises the exception Failure. *)
let test_raises_some_failure f x =
  test_raises_exc_p (function Failure _ -> true | _ -> false) f x
;;

let failure_test f x s = test_raises_this_failure s f x
and any_failure_test = test_raises_some_failure
;;
