(******************************************************************************
 *                             Core-extended                                  *
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

open Core.Std
(** Extensions to [Sexplib.Sexp].*)

val is_atom : Sexp.t -> bool
val is_list : Sexp.t -> bool

(** {3 Constructors } *)

val atom : string -> Sexp.t
val list : Sexp.t list -> Sexp.t

(**{3 Printing }*)
(**
   The ocaml pretty printer (used by sexplib) is a speed daemon  but is,
   sadly enough, produces wrong output (e.g it overflows in places where this
   could have avoided). This uses a printer from wadler's a prettier printer to
   output strings suited to human consumption.
*)
val to_string_hum' : Sexp.t -> string

val format : Sexp.t -> Pp.t

(**
   A more readable but less compact pretty printer than the one bundled by
   sexplib. This is going through a test period at which point it might
   make it in sexplib. It uses ocaml's pretty-printing library so it is both
   fast and broken.
*)
val pp_hum' : Format.formatter -> Sexp.t -> unit

(** Takes a string and returns the same string but commented according to
    sexp's syntax*)
val comment : string -> string

(** {3 Various} *)

val print_diff : ?oc:out_channel -> Sexp.t -> Sexp.t -> unit

(** Returns a smaller sexp by replacing sections with "...".  Will try to show parts of the
   sexp "near" sub_sexp.

   Limiting size to length a string length is less efficient than a certain depth.  The
   meaning of a given depth is arbitrary except that more depth gives you a bigger sexp.  Try
   100 or so. *)
val summarize : Sexp.t -> sub_sexp:Sexp.t -> size:[ `depth of int | `string of int ] -> Sexp.t

(** [of_sexp_allow_extra_fields of_sexp sexp] uses [of_sexp] to convert [sexp] to a
    value, but will not fail if there any extra fields in a record.  *)
val of_sexp_allow_extra_fields : (Sexp.t -> 'a) -> Sexp.t -> 'a

(** {3 Transforming sexp parsers} *)

(* [filter_record t_of_sexp field_names] will return a new [t_of_sexp] function
   which will take sexps representing records and ignore all fields not
   mentioned in [field_names] (which can be given by [Fields.names]).

   The motivation for this is to let config files (based on a record type) to
   gain new fields and still work with old code. The old code will ignore the
   new fields and still be able to parse the old config type from the new sexp.
*)
val filter_record : (Sexp.t -> 'a) -> string list -> (Sexp.t -> 'a)

module Records_table : sig
  (* Given 2 types:
     type t1 = {
     symbol : string;
     contracts_open : int;
     }
     type t2 = {
     symbol : string;
     contracts_open : int;
     description : string;
     }

     let t1_of_sexp = t_of_sexp t1_of_sexp
     let t2_of_sexp = t_of_sexp t2_of_sexp

     Both functions will successfully parse a sexp of the form:

     ((symbol     contracts_open    description)
     (ABC           100            "...")
     (XYZ           350            "..."))
  *)
  type 'a t = 'a list

  include Sexpable.S1 with type 'a sexpable = 'a t
end
