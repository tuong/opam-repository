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

(** Toggle to turn on/off checking for descriptor leaks at exit (default: on) *)
val run_check_at_exit : bool ref

(** Fraction of maximum number of descriptors considered critical.
    Default: 0.9 *)
val critical : float ref

(** [report_open_files ()] prints a dump of open file descriptors to [stderr]
    in two formats, one using the proc file system, the other by executing
    [/usr/sbin/lsof] in a child process. *)
val report_open_files : unit -> unit

(** [report_on_exn exn] calls {!report_open_files} iff [exn] indicates a file
    descriptor leak (Unix error with code EMFILE or ENFILE). *)
val report_on_exn : exn -> unit

(** [percent_fds_in_use ()] reports the percentage of fds that are in use. *)
val percent_fds_in_use : unit -> float
