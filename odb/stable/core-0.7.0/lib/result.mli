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

(** [Result] is often used to handle error messages. *)

(** ['a] is a function's expected return type, and ['b] is often an error message string.
  {[let ric_of_ticker = function
      | "IBM" -> Ok "IBM.N"
      | "MSFT" -> Ok "MSFT.OQ"
      | "AA" -> Ok "AA.N"
      | "CSCO" -> Ok "CSCO.OQ"
      | _ as ticker -> Error (sprintf "can't find ric of %s" ticker) ]}
    The return type of ric_of_ticker could be [string option], but [(string, string)
    Result.t] gives more control over the error message. *)
type ('ok, 'err) t =
  | Ok of 'ok
  | Error of 'err

include Sexpable.S2 with type ('a,'err) sexpable = ('a,'err) t
include Binable.S2 with type ('a,'err) binable = ('a,'err) t
include Monad.S2 with type ('a,'err) monad = ('a,'err) t

val fail : 'err -> (_, 'err) t

(** e.g. [failf "Couldn't find bloogle %s" (Bloogle.to_string b)] *)
val failf : ('a, unit, string, (_, string) t) format4 -> 'a

val is_ok : (_, _) t -> bool
val is_error : (_, _) t -> bool

val ok : ('ok, _) t -> 'ok option
val error : (_, 'err) t -> 'err option

val of_option : 'ok option -> error:'err -> ('ok, 'err) t

val iter : ('ok, _) t -> f:('ok -> unit) -> unit
val map : ('ok, 'err) t  -> f:('ok -> 'c) -> ('c, 'err) t
val map_error : ('ok, 'err) t  -> f:('err -> 'c) -> ('ok, 'c) t

(* these two are rarely used *)
val call : f:(('a -> unit), _) t -> 'a -> unit
val apply : f:(('a -> 'b), 'err) t -> 'a -> ('b, 'err) t

(** [ok_fst] is useful with [List.partition_map].  Continuing the above example:
{[
    let rics, errors = List.partition_map ~f:Result.ok_fst
      (List.map ~f:ric_of_ticker ["AA"; "F"; "CSCO"; "AAPL"]) ]} *)
val ok_fst : ('ok, 'err) t -> [ `Fst of 'ok | `Snd of 'err ]


(* [ok_if_true] returns [Ok ()] if [bool] is true, and [Error error] if it is false *)
val ok_if_true : bool -> error : 'err -> (unit, 'err) t

val try_with : (unit -> 'a) -> ('a, exn) t

(** [ok_exn t] returns [x] if [t = Ok x], otherwise it raises an exn. *)
val ok_exn : ?fail:exn -> ('ok, _) t -> 'ok

(** [raise_error t] returns [x] if [t = Ok x], and raises [exn] if [t = Error
    exn] *)
val raise_error : ('ok, exn) t -> 'ok



module Export : sig
  type ('ok, 'err) _result =
    ('ok, 'err) t =
  | Ok of 'ok
  | Error of 'err
end
