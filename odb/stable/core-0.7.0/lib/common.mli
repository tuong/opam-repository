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

(** Basic types and definitions required throughout the system. *)

exception Bug of string

(** Raised when finalization after an exception failed, too.
    The first exception argument is the one raised by the initial
    function, the second exception the one raised by the finalizer. *)
exception Finally of exn * exn

exception Validation_error of string list

exception Unimplemented of string


(* The sexps of this type only have 12 digits after the decimal. Therefore they're less
   annoying to look at than the sexps of floats. The input sexps disallow nan and inf. *)
type decimal = float with bin_io, sexp


type 'a bound = Incl of 'a | Excl of 'a | Unbounded


type passfail = Pass | Fail of string

(** Handy types for marking things read-only and read-write. One should not expose
    functions for converting between read_only/immutable/read_write because the private
    types expose the subtyping. Users would say "(db :> read_only Db.t)" to cast. The
    difference between read-only and immutable is that someone else can change a read-only
    object, while immutable never changes. *)
type read_only with sexp, bin_io
type immutable = private read_only with sexp, bin_io
type read_write = private read_only with sexp, bin_io
type write_only with sexp, bin_io

(** [never_returns] should be used as the return type of functions that don't return and
 * might block forever, rather than ['a] or [_].  This forces callers of such functions to
 * have a call to [never_returns] at the call site, which makes it clear to readers what's
 * going on. We do not intend to use this type for functions such as [failwithf] that
 * always raise an exception. *)
type never_returns
val never_returns : never_returns -> _

(** {6 Error handling} *)
(** See exn.mli *)
val protectx : f:('a -> 'b) -> 'a -> finally:('a -> unit) -> 'b
val protect : f:(unit -> 'a) -> finally:(unit -> unit) -> 'a


val critical_section : Mutex.t -> f:(unit -> 'a) -> 'a

(** {6 Input Output}*)



(** [read_wrap ~f fname] executes [~f] on the open input channel from
    [fname], and closes it afterwards.

    [read_wrap] opens the channel in binary mode iff [binary] is true, and
    only has an effect on Windows.
 *)
val read_wrap : ?binary:bool -> f:(in_channel -> 'a) -> string -> 'a


(** [write_wrap ~f fname] executes [~f] on the open output channel from
    [fname], and closes it afterwards.

    [write_wrap] opens the channel in binary mode iff [binary] is true, and
    only has an effect on Windows. *)
val write_wrap : ?binary:bool -> f:(out_channel -> 'a) -> string -> 'a


(** [write_lines fname lines] writes each string in [lines] (plus a newlnie) to file
    [fname]. *)
val write_lines : string -> string list -> unit


(** Completely reads an input channel and returns the results as a list of
    strings. Each line in one string. *)
val input_lines : ?fix_win_eol:bool -> in_channel -> string list


(** [read_lines filename] Opens filename, reads all lines, and closes the file. *)
val read_lines : string -> string list

(**{6 triple handling }*)
val fst3 : ('a * _  * _ ) -> 'a
val snd3 : (_  * 'a * _ ) -> 'a
val trd3 : (_  * _  * 'a) -> 'a

(**
   {6 Option handling}
*)

val uw : 'a option -> 'a

val is_none : 'a option -> bool
val is_some : 'a option -> bool

(** {6 Functions from function.ml} *)
val (|!) : 'a -> ('a -> 'b) -> 'b

val ident : 'a -> 'a
val const : 'a -> _ -> 'a

(** A comparator that returns results in ascending order. *)
external ascending : 'a -> 'a -> int = "%compare"
(** A comparator that returns results in descending order. *)
val descending : 'a -> 'a -> int

(** same as [Filename.concat]*)
val (^/) : string -> string -> string

val failwithf    : ('a, unit, string, unit -> _) format4 -> 'a
val invalid_argf : ('a, unit, string, unit -> _) format4 -> 'a
val exitf        : ('a, unit, string, unit -> _) format4 -> 'a

(* [with_return f] allows for something like the return statement in C within [f].  There
   are three ways [f] can terminate:

   1. If [f] calls [r.return x], then [x] is returned by [with_return].
   2. If [f] evaluates to a value [x], then [x] is returned by [with_return].
   3. If [f] raises an exception, it escapes [with_return].

   Here is a typical example:

   {[
    let find l ~f =
      with_return (fun r ->
        List.iter l ~f:(fun x -> if f x then r.return (Some x));
        None)
   ]}

   It is only because of a deficiency of ML types that [with_return] doesn't have type:

   {[ val with_return : 'a. (('a -> ('b. 'b)) -> 'a) -> 'a ]}

   but we can slightly increase the scope of 'b, without changing the meaning of the type
   and then we get
   type 'a return = { return : 'b . 'a -> b }
   val with_return : ('a return -> 'a) -> 'a

   But the actual reason we chose to use a record type with polymorphic field is that otherwise we
   would have to clobber the namespace of functions with [return] and that is undesirable
   because [return] would get hidden as soon as we open any monad. We considered names
   different than [return] but everything seemed worse than just having [return] as a
   record field. We are clobbering the namespace of record fields but that is much more acceptable.
*)

type 'a return = private { return : 'b. 'a -> 'b }
val with_return : ('a return -> 'a) -> 'a

(** toplevel binding for polymorphic equality (=).  Named for easy use in
    labelled arguments (one can do [f x y ~equal]).
*)
val equal : 'a -> 'a -> bool

(* We disable [==] and [!=] and replace them with the longer and more mnemonic
   [phys_equal] because they too easily lead to mistakes (for example
   they don't even work right on Int64 or Float).  One can usually use the
   [equal] function for a specific type, or use (=) or (<>) for built in types
   like char, int, float, ...
*)
val phys_equal : 'a -> 'a -> bool
val (==) : 'a -> 'a -> [ `Consider_using_phys_equal ]
val (!=) : 'a -> 'a -> [ `Consider_using_phys_equal ]

val force : 'a Lazy.t -> 'a



(* override Pervasives methods that need LargeFile support *)
val seek_out : [ `Deprecated_use_out_channel ]
val pos_out : [ `Deprecated_use_out_channel ]
val out_channel_length : [ `Deprecated_use_out_channel ]
val seek_in : [ `Deprecated_use_in_channel ]
val pos_in : [ `Deprecated_use_in_channel ]
val in_channel_length : [ `Deprecated_use_in_channel ]
val modf : [ `Deprecated_use_float_modf ]
val truncate : [ `Deprecated_use_float_round_towards_zero ]


(** we have our own version of these two, the INRIA version doesn't release the runtime
    lock.  *)
val close_in : in_channel -> unit
val close_out : out_channel -> unit
