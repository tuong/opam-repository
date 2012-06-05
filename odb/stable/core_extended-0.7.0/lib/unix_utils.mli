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

(** Interface to Unix utility functions *)

open Core.Std
open Unix

(** {2 Handling RAM limits} *)

val physical_ram : unit -> int64
(** [physical_ram ()] @return the total amount of physical RAM in bytes. *)

val ram_limit_spec : Arg.t
(** [ram_limit_spec] command line arguments to set ram limits. *)


(** {2 Signal handling} *)

val wrap_block_signals : (unit -> 'a) -> 'a
(** [wrap_block_signals f] blocks all signals before execution of [f], and
    restores them afterwards. *)

val ensure_at_exit : unit -> unit
(** [ensure_at_exit ()]: catch all signals, run at_exit functions,
    then re-deliver the signal to self to ensure the default behavior.
    at_exit functions are honored only when terminating by exit, not by signals,
    so we need to do some tricks to get it run by signals too*)
