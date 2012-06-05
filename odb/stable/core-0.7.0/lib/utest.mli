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

(** Lightweight module for registering and running unit tests  *)

(** This module can be used to safely expose functions and values in signatures
    that can only be used in unit tests.

    Under the hood, ['a t = (unit -> 'a)] and the only thing that ever forces them
    is the [run_tests_and_exit] function below which actually runs the unit tests.

    For example, suppose in some module, [type t] is actually an [int].  You
    want to keep the type definition opaque, but use the underlying
    representation in unit tests.  You could write in the ml:

    {[let test_to_int t = Only_in_test.return t
    [let test_of_int n = Only_in_test.return n]}

    You would then expose in the mli:

    {[type t
    ... functions for use in regular programming...
    val test_to_int : t -> int Only_in_test.t
    val test_of_int : int -> t Only_in_test.t]}

    Finally, if you have specific values that you might want to use in unit
    tests, but that have top-level side-effects or take too long to compute, you
    can delay the side-effects or computation until the unit tests are run by
    writing, e.g.:

    [let (test_special_value : t Only_in_test.t) =
       Only_in_test.of_thunk (fun () ->  (factorial 100))]

    instead of

    [let (test_special_value : t Only_in_test.t) =
       Only_in_test.return (factorial 100)]
*)

module Only_in_test : sig
  type 'a t
  include Monad.S with type 'a monad = 'a t
  val of_thunk : (unit -> 'a) -> 'a t
  val all : 'a t list -> 'a list t
end

module type S = sig
  (** [register ?get_location test] registers a unit-test that triggers errors
      via asserts (either regular or via the Asserts library).

      (The [get_location] argument is a hack; if you pass in [(fun () -> assert
      false)], it will extract the location of the test from the assertion
      failure.)

      Example use (supposing the functions in the comment above were defined in
      some module [M]):

      {[let () = UTest.register ~get_location:(fun () -> assert false)) (
      let (>>=) = Only_in_test.(>>=) in
      let (>>|) = Only_in_test.(>>|) in
      M.test_of_int 23 >>= fun m ->
      M.test_special_value >>= fun special_m ->
      ...
      assert (condition1);
      assert (condition2);
      ...
      )]}

      When [run_tests_and_exit] is run, all unit tests that have been registered will be
      run.  Any assertion failures will be reported, together with their
      locations.

      If you pass in a [file_name], you will be able to run all tests registered
      with the same file_name in a batch; similarly with [test_name].  See the
      comment on [run_tests_and_exit] below.  *)
  val register : ?get_location:(unit -> unit) -> unit Only_in_test.t -> unit

  (** [register_thunk] is the same as [register], except that it applies
      [Only_in_test.of_thunk] to the test for you. *)
  val register_thunk : ?get_location:(unit -> unit) -> (unit -> unit) -> unit

  (** [run_tests_and_exit] Run all tests registered with [register].  Statistics
      are reported, and [exit] is called after it's done.  It exits with status
      1 if any test fails (i.e., any assertion failure or other exception is
      raised in the running of a test.)
  *)
  val run_tests_and_exit : unit -> unit

  (** Same as [run_tests] but returns false if one of the tests fails *)
  val run_tests : unit -> bool
end

module Make (Dummy : sig end) : S

(** [debug_printf] is meant to be used in unit tests.  It will be printed just in case
    the test fails with an assert failure.  *)
val debug_printf : ('a, unit, string, unit) format4 -> 'a

(** This functor will give you comparison functions which automatically call debug_printf
    when they are called with what they were called with and their result *)
module Debug_compare (S:sig
  type t with sexp
  val compare : t -> t -> int
end) : sig
  val equal      : S.t -> S.t -> bool
  val compare    : S.t -> S.t -> int
  val ascending  : S.t -> S.t -> int
  val descending : S.t -> S.t -> int
  val min        : S.t -> S.t -> S.t
  val max        : S.t -> S.t -> S.t
  val ( >= )     : S.t -> S.t -> bool
  val ( <= )     : S.t -> S.t -> bool
  val ( = )      : S.t -> S.t -> bool
  val ( > )      : S.t -> S.t -> bool
  val ( < )      : S.t -> S.t -> bool
  val ( <> )     : S.t -> S.t -> bool
end
