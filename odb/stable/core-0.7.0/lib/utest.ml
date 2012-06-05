(******************************************************************************
 *                             Core                                           *
 *                                                                            *
 * Copyright (C) 2008- Jane Street Holding, LLC                               *
 *    Contact: opensource@janestreet.com                                      *
 *    WWW: http://www.janestreet.com/ocaml                                    *
 *                                                                            *
 *                                                                            *
 * This library is free software; you can redistribute it and/or              *
 * modify it under the terms of the GNU Lesser General Public                 *
 * License as published by the Free Software Foundation; either               *
 * version 2 of the License, or (at your option) any later version.           *
 *                                                                            *
 * This library is distributed in the hope that it will be useful,            *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 * Lesser General Public License for more details.                            *
 *                                                                            *
 * You should have received a copy of the GNU Lesser General Public           *
 * License along with this library; if not, write to the Free Software        *
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                            *
 ******************************************************************************)

open Result.Export
module List = StdLabels.List

module Only_in_test = struct
  type 'a t = (unit -> 'a)
  include Monad.Make (struct
    type 'a z = 'a t
    type 'a t = 'a z
    let return x = (fun () -> x)
    let bind x f = (fun () -> f (x ()) ())
  end)
  let of_thunk x = x

  let all ts = List.fold_left ts ~init:(return []) ~f:(fun acc t ->
    acc >>= fun acc -> t >>| fun x -> x::acc)
end

module type S = sig
  val register : ?get_location:(unit -> unit) -> unit Only_in_test.t -> unit
  val register_thunk : ?get_location:(unit -> unit) -> (unit -> unit) -> unit
  val run_tests_and_exit : unit -> unit
  val run_tests : unit -> bool
end

module Location = struct
  type t = {
    file : string;
    line : int;
    char : int;
  }

  let opt_to_string = function
    | None -> "unknown location"
    | Some loc -> Printf.sprintf "file %s line %d char %d" loc.file loc.line loc.char
end

module Assert_failure = struct
  type t = {
    location : Location.t;
    backtrace : string option;
  }
  let extract f =
    try f (); None with
    | Assert_failure (file,line,char) ->
      let backtrace =
        match Exn.backtrace () with
        | "" -> None
        | s -> Some s
      in
      Some {
        location =
          {
            Location.
            file = file;
            line = line;
            char = char;
          };
        backtrace = backtrace;
      }

    let to_string t =
      let backtrace =
        match t.backtrace with
        | None -> ""
        | Some s -> "\n" ^ s
      in
      (Location.opt_to_string (Some t.location)) ^ backtrace
end


module Test = struct
  type t = {
    location : Location.t option;
    test : (unit -> unit);
  }

  let create ~get_location test =
    let module A = Assert_failure in
    let location = Option.map (A.extract get_location) ~f:(fun a -> a.A.location) in
    { location = location;
      test = test;
    }

  let run t =
    Result.try_with (fun () ->
      match Assert_failure.extract t.test with
      | None -> `Pass
      | Some a -> `Fail a
    )

end

module Tests = Caml.Map.Make (struct
  type t = string option
  let compare = compare
end)

module Output = Caml.Set.Make (struct
  type t = string
  let compare = compare
end)

let output = Queue.create ()
let debug_printf f = Printf.ksprintf (fun s -> Queue.add s output) f

module Debug_compare (S:sig
  type t with sexp
  val compare : t -> t -> int
end) = struct
  module S' = Sexpable.To_stringable (struct
    include S
    type sexpable = t
  end)

  let lift result_to_string ~name f x y =
    let res = f x y in
    debug_printf "%s %s %s = %s"
      name (S'.to_string x) (S'.to_string y) (result_to_string res);
    res

  let lift_int  ~name f x y = lift string_of_int  ~name f x y
  let lift_bool ~name f x y = lift string_of_bool ~name f x y
  let lift_x    ~name f x y = lift S'.to_string   ~name f x y

  let compare    = lift_int ~name:"compare"    S.compare
  let ascending  = lift_int ~name:"ascending"  S.compare
  let descending = lift_int ~name:"descending" (fun x y -> S.compare y x)

  let equal = lift_bool ~name:"equal" (fun a b -> S.compare a b = 0)
  let min = lift_x ~name:"min" (fun t t' -> if S.compare t t' <= 0 then t else t')
  let max = lift_x ~name:"max" (fun t t' -> if S.compare t t' >= 0 then t else t')

  let (>)  = lift_bool ~name:"(>)"  (fun a b -> S.compare a b >  0)
  let (<)  = lift_bool ~name:"(<)"  (fun a b -> S.compare a b <  0)
  let (>=) = lift_bool ~name:"(>=)" (fun a b -> S.compare a b >= 0)
  let (<=) = lift_bool ~name:"(<=)" (fun a b -> S.compare a b <= 0)
  let (=)  = lift_bool ~name:"(=)"  (fun a b -> S.compare a b =  0)
  let (<>) = lift_bool ~name:"(<>)" (fun a b -> S.compare a b <> 0)
end

let verbose = false

let run_tests tests =
  let some_test_failed = ref false in
  if Tests.is_empty tests
  then Printf.printf "No tests to run!\n"
  else Tests.iter (fun file tests ->
    
    Printf.printf "---%s---\n" (Option.value file ~default:"unknown file");
    let tests = List.rev tests in
    let num_tests = List.length tests in
    let i = ref (-1) in
    List.iter tests ~f:(fun test ->
      incr i;
      let i = !i in
      let test_index_string = Printf.sprintf "Test %d of %d" (i + 1) num_tests in
      let main_test_location = Location.opt_to_string test.Test.location in
      Queue.clear output;
      match Test.run test with
      | Ok `Pass ->
        if verbose then
          Printf.printf "%s at %s passed\n"
            test_index_string main_test_location
      | Ok (`Fail assert_failure) ->
        some_test_failed := true;
        Printf.printf "\027[31m%s: failure at %s\027[0m\n"
          test_index_string (Assert_failure.to_string assert_failure);
        if not (Queue.is_empty output)
        then begin
          Printf.printf "Output:\n";
          Queue.iter print_endline output;
        end
      | Error exn ->
        some_test_failed := true;
        Printf.printf "\027[31m%s at %s raised exception %s\027[0m\n"
          test_index_string main_test_location (Exn.to_string exn)
    )
  ) tests;
  not !some_test_failed

module Make (Dummy : sig end) = struct

  let tests = ref Tests.empty

  let register ?(get_location=(fun () -> ())) test =
    let test = Test.create ~get_location test in
    let file = Option.map test.Test.location ~f:(fun loc -> loc.Location.file) in
    let loc_tests = try Tests.find file !tests with _ -> [] in
    tests := Tests.add file (test::loc_tests) !tests

  let register_thunk = register

  let run_tests () = run_tests !tests

  let run_tests_and_exit () =
    let ok = run_tests () in
    Pervasives.exit (if ok then 0 else 1)
end
