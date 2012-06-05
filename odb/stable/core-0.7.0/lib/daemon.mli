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

(** [daemonize ?(close_stdio = true) ?(cd = "/") ?umask=[0] ()] makes the current
    executing process a daemon, and dups /dev/null to stdin/stdout/stderr if
    close_stdio=true. See Chapter 13 of Advanced Programming in the UNIX Environment
    Second Edition by Stephens and Rago for more details.

    @raise Failure if fork was unsuccessful.
*)
val daemonize :
  ?close_stdio : bool
  -> ?cd : string
  -> ?umask : int
  -> unit
  -> unit

(** [daemonize_wait ?(cd = "/") ?(umask=0) ()] makes the executing process a
    daemon, but delays full detachment from the calling shell/process until
    the returned "release" closure is called.

    Any output to stdout/stderr before the "release" closure is called will get
    sent out normally.  After "release" is called, /dev/null gets dup'd to
    stdin/stdout/stderr.

    If the process exits before the "release" closure is called, the exit code
    will bubble up to the calling shell/process.

    Note that calling the release closure will adjust SIGPIPE
    handling, so you should not rely on the delivery of this signal
    during this time.

    @raise Failure if fork was unsuccessful.
*)
val daemonize_wait : ?cd : string -> ?umask : int -> unit -> (unit -> unit)
