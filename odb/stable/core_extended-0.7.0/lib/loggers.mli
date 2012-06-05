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

(** High-level logging capabilities

    @author Markus Mottl <mmottl\@janestreet.com>
*)

(** {2 Interface specification of loggers} *)

(** Type of logging functions *)
type 'a logger = ('a, unit, string, unit) format4 -> 'a

(** Interface to loggers *)
module type LOGGER = sig
  (** {3 Setting and accessing log levels} *)

  (** Set minimum log level *)
  val set_lev : Syslog.lev -> unit

  (** Get minimum log level *)
  val get_lev : unit -> Syslog.lev

  (** Test whether a log level may be logged. *)
  val may_log : Syslog.lev -> bool


  (** {3 Logging functions} *)

  val generic : Syslog.lev -> 'a logger

  val debug : 'a logger
  val info : 'a logger
  val notice : 'a logger
  val warning : 'a logger
  val err : 'a logger
  val crit : 'a logger
  val alert : 'a logger
  val emerg : 'a logger
end

(** Specification for creating loggers *)
module type SPEC = sig
  val logger : Syslog.lev -> string -> unit
end

(** Functor for creating loggers *)
module MakeLogger (Spec : SPEC) : LOGGER


(** {2 Logger implementations} *)

(** Functor for creating channel loggers *)
module MakeChannel (ChannelSpec : sig val oc : out_channel end) : LOGGER

(** Logs to [stderr] *)
module Stderr : LOGGER

(** Logs to syslog daemon *)
module Syslog : LOGGER

(** Ignores logging messages *)
module Ignore : LOGGER
