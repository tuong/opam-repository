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

(** Code to test the effect of exceptions happening in strategic
    places in daemons.

    In order to use this module one defines a list
    of (mnemonic, exception) pairs

    E.G.

    [("M.f: invalid arg", Invalid_argument "foo");
     ("Z.f: sys error", Sys_error "bar");
     ("R.z: failure", Failure "baz")]

    And one passes this list to create. Then one places calls to
    Exception_check.maybe_raise <name>, in important parts of one's
    code. When the code is run, it will listen on [listen_port], and
    one can connect with netcat and type a name, which will
    cause that exception to be raised on the next call to
    [Exception_check.maybe_raise]. *)

(** create should not be called more than once *)
val create : ?listen_port:int -> (string * exn) list -> unit

(** [maybe_raise name] if the exception associated with any name in [name]
    has been triggered, then raise it, otherwise do nothing. Only the
    first exception in the list will be raised. This function is
    thread safe. *)
val maybe_raise : string list -> unit
