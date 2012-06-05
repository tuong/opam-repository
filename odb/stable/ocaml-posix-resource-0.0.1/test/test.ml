(******************************************************************************)
(*  ocaml-posix-resource: POSIX resource operations                           *)
(*                                                                            *)
(*  Copyright (C) 2009 Sylvain Le Gall <sylvain@le-gall.net>                  *)
(*                                                                            *)
(*  This library is free software; you can redistribute it and/or modify it   *)
(*  under the terms of the GNU Lesser General Public License as published by  *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at   *)
(*  your option) any later version; with the OCaml static compilation         *)
(*  exception.                                                                *)
(*                                                                            *)
(*  This library is distributed in the hope that it will be useful, but       *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of                *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser  *)
(*  General Public License for more details.                                  *)
(*                                                                            *)
(*  You should have received a copy of the GNU Lesser General Public License  *)
(*  along with this library; if not, write to the Free Software Foundation,   *)
(*  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA               *)
(******************************************************************************)

open POSIXResource;;
open OUnit;;

let all_resources = 
  [
    RLIMIT_CORE;
    RLIMIT_CPU;
    RLIMIT_DATA;
    RLIMIT_FSIZE;
    RLIMIT_NOFILE;
    RLIMIT_STACK;
    RLIMIT_AS
  ]
;;

let string_of_resource =
  function
    | RLIMIT_CORE   -> "RLIMIT_CORE"
    | RLIMIT_CPU    -> "RLIMIT_CPU"
    | RLIMIT_DATA   -> "RLIMIT_DATA"
    | RLIMIT_FSIZE  -> "RLIMIT_FSIZE"
    | RLIMIT_NOFILE -> "RLIMIT_NOFILE"
    | RLIMIT_STACK  -> "RLIMIT_STACK"
    | RLIMIT_AS     -> "RLIMIT_AS"
;;

let all_tests =
  "posix_resource" >:::
  [
    "getrlimit" >:::
    (List.rev_map 
       (fun rsrc ->
          (string_of_resource rsrc) >::
          (fun () ->
             let _soft, _hard =
               getrlimit rsrc
             in
               ()))
       all_resources);

    "setrlimit" >:::
    (List.flatten 
      (List.rev_map
         (fun (rsrc, lmts) ->
            (List.rev_map
               (fun (cslmt, chlmt, todo) ->
                  (Printf.sprintf "Limit %s to %s (soft) and %s (hard)"
                     (string_of_resource rsrc)
                     (string_of_change_limit cslmt)
                     (string_of_change_limit chlmt)) >::
                  (fun () ->
                     try 
                       let () = 
                         if todo then 
                           OUnit.todo "Unclear how it works"
                       in
                       let exp_slmt = 
                         match cslmt with 
                           | No_change -> fst (getrlimit rsrc)
                           | Limit lmt -> lmt
                       in
                       let exp_hlmt = 
                         match chlmt with 
                           | No_change -> snd (getrlimit rsrc)
                           | Limit lmt -> lmt
                       in
                       let res_slmt, res_hlmt = 
                         setrlimit rsrc cslmt chlmt;
                         getrlimit rsrc
                       in
                         assert_equal 
                           ~printer:Limit.to_string
                           ~msg:"soft limit"
                           ~cmp:Limit.equal
                           exp_slmt
                           res_slmt;
                         assert_equal
                           ~printer:Limit.to_string
                           ~msg:"hard limit"
                           ~cmp:Limit.equal
                           exp_hlmt
                           res_hlmt
                     with Unix.Unix_error (code, func, _) ->
                       failwith
                         (Printf.sprintf
                            "%s: %s"
                            func
                            (Unix.error_message code))))
               lmts))
         [
           RLIMIT_CORE,   [No_change,              No_change,               false; 
                           Limit None,             No_change,               false; 
                           Limit (Some 10000L),    No_change,               false; 
                           Limit (Some 4096L),     No_change,               false; 
                           Limit (Some 0L),        No_change,               false];
           RLIMIT_CPU,    [Limit (Some 1000L),     No_change,               true;
                           Limit (Some 1000L),     Limit (Some 2000L),      true];
           RLIMIT_DATA,   [No_change,              No_change,               true;
                           Limit (Some 16777216L), No_change,               false];
           RLIMIT_FSIZE,  [No_change,              No_change,               true;
                           Limit (Some 16777216L), No_change,               false;
                           No_change,              Limit (Some 16777216L),  true];
           RLIMIT_NOFILE, [Limit (Some 512L),      Limit (Some 1024L),      true];
           RLIMIT_STACK,  [No_change,              No_change,               true;
                           Limit (Some 16777216L), No_change,               false];
           RLIMIT_AS,     [No_change,              No_change,               true;
                           Limit (Some 16777216L), No_change,               false];
         ]));
    "getpriority" >::
    (fun () ->
       let _i : int = 
         getpriority (PRIO_PROCESS (Unix.getpid ()))
       in 
         ());

    "setpriority" >::
    (fun () ->
       let me = 
         PRIO_PROCESS (Unix.getpid ())
       in
       let prio = 
         getpriority me
       in 
         setpriority me (prio + 1);
         assert_equal 
           ~printer:string_of_int
           (prio + 1)
           (getpriority me));
  ]
;;

let res =
  run_test_tt all_tests
in
  List.iter
    (function
       | RFailure _ | RError _ -> exit 1
       | _ -> ())
    res
;;
