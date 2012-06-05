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

(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
open Core.Std
open Unix

type uids = {
  ruid:int;
  euid:int;
  suid:int
} with sexp,bin_io

val setresuid : ?ruid:int -> ?euid:int -> ?suid:int -> unit -> unit
val getresuid : unit -> uids

(** {6 Splicing - zero-copies between kernel buffers} *)

(* Example usage diagram:

   In the below diagram, starting at the left upper corner, we first
   splice a socket into a pipe.  Then we duplicate this pipe into two
   other pipes using "tee".  The first pipe is spliced into a file
   descriptor (e.g. to log data coming in from a socket connection to
   a file).  The second pipe is used by the user to actually read data.

   After handling the received data, the user puts the new data to
   be sent out into an output buffer and vmsplices it into a pipe.
   Use double buffering ( = switching between two buffers) to prevent
   data inconsistencies while the kernel may be reading from the user
   provided pages, and make sure not to let buffers be reclaimed by the
   GC as long as they may still be in use!  These buffers currently need
   not be larger than 64KB each, which is the size of kernel buffers
   (= Unix-pipes).

   The end of the output pipe is then duplicated into two more output
   pipes using "tee" again.  If these two "tees" have seen all the data
   vmspliced from a user buffer, the user can safely switch to it again
   from the other double buffer.

   Finally, the first pipe is used to splice data into a socket to send
   out the user data, and the second pipe is used to stream this data
   to a file.  Note that using more pipes and more "tee"-calls one can
   very cheaply duplicate the data to even more destinations!

                       tee              splice
                           +----> pipe ----+---> fd
             splice       /
     socket ----+---> pipe
                          \             read
                           +----> pipe ---+--> user input space
                       tee                       |
                                                 + do stuff
                                                 |
                                               user output space
                                                 |
                                                 + vmsplice (double buffer!)
                                                 |
                                                pipe
                                                 |
                                                / \
                                           tee +   + tee
                                               |   |
                                             pipe pipe
                                             /       \
                                     splice +         + splice
                                           /           \
                                         sock           fd
*)

module Splice : sig
  (** {6 Splice flags} *)

  (** Type of Splice event flag *)
  type flag = MOVE | NONBLOCK | MORE | GIFT with sexp, bin_io

  (** Type of Splice event flags *)
  type flags

  val make_flags : flag array -> flags
  (** [make_flags ar] @return flags constructed from the array of flags [ar]. *)


  (** {6 Splice functions} *)

  val splice :
    ?assume_fd_is_nonblocking : bool ->
    fd_in : file_descr -> ?off_in : int ->
    fd_out : file_descr -> ?off_out : int ->
    len : int ->
    flags
    -> int * int * int
  (** [splice ?assume_fd_is_nonblocking ~fd_in ?off_in ~fd_out ?off_out
      ~len flags] see man-page for details.  @return the triple [(ret,
      ret_off_in, ret_off_out)], where [ret] corresponds to the return
      value of the system call, [ret_off_in] to the final input offset,
      and [ret_off_out] to the final output offset.

      @raise Unix_error on Unix-errors.
      @raise Invalid_argument if the offsets or length are invalid

      @param assume_fd_is_nonblocking default = false
      @param off_in default = 0
      @param off_out default = 0
  *)

  val tee :
    ?assume_fd_is_nonblocking : bool ->
    fd_in : file_descr -> fd_out : file_descr -> int -> flags -> int
  (** [tee ?assume_fd_is_nonblocking ~fd_in ~fd_out len flags] see man-page
      for details.

      @raise Unix_error on Unix-errors.
      @raise Invalid_argument if the length is invalid

      @param assume_fd_is_nonblocking default = false
  *)

  val vmsplice :
    ?assume_fd_is_nonblocking : bool ->
    file_descr -> Bigstring.t IOVec.t array -> ?count : int -> flags -> int
  (** [vmsplice ?assume_fd_is_nonblocking fd iovecs ?count flags]
      see man-page for details.

      @raise Unix_error on Unix-errors.
      @raise Invalid_argument if the count is invalid

      @param assume_fd_is_nonblocking default = false
      @param count default = [Array.length iovecs]
  *)
end
