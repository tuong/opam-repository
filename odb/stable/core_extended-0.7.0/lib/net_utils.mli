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

(** Networking utilities

    @author Markus Mottl <mmottl\@janestreet.com>
*)

open Unix

(** Exception raised if a connection attempt timed out *)
exception Timeout

(** Get hostname from sockaddr *)
val h_name_of_sockaddr : sockaddr -> string

(** Get string from sockaddr *)
val string_of_sockaddr : sockaddr -> string

(** Get hostname or (on Not_found) inet string from sockaddr *)
val h_name_or_string_of_sockaddr : sockaddr -> string

(** Get inet_addr from sockaddr *)
val inet_addr_of_sockaddr : sockaddr -> inet_addr

(** Get port from sockaddr *)
val port_of_sockaddr : sockaddr -> int

(** Get port from a socket associated with an [in_channel] *)
val port_of_in_channel : in_channel -> int

(** Create a standard TCP/IP-socket *)
val tcp_socket : unit -> file_descr

(** Connect a socket with a connect timeout *)
val connect_tmout : file_descr -> sockaddr -> float -> bool

(** Create a socket with timeouts *)
val open_fd_connection_tmout :
  con_timeout : float ->
  rcv_timeout : float ->
  snd_timeout : float ->
  sockaddr -> file_descr

(** Open a connection with timeouts *)
val open_connection_tmout :
  con_timeout : float ->
  rcv_timeout : float ->
  snd_timeout : float ->
  sockaddr -> in_channel * out_channel

(** Set a timeout for a socket associated with an [in_channel] *)
val set_in_channel_timeout : in_channel -> float -> unit

(** Set a timeout for a socket associated with an [out_channel] *)
val set_out_channel_timeout : out_channel -> float -> unit
