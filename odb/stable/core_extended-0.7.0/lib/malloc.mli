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

(** Malloc bindings

    Allows you to set/query the behaviour of malloc.
*)

type mallinfo = {
  arena : int;  (** non-mmapped space allocated from system *)
  ordblks : int;  (** number of free chunks *)
  smblks : int;  (** number of fastbin blocks *)
  hblks : int;  (** number of mmapped regions *)
  hblkhd : int;  (** space in mmapped regions *)
  usmblks : int;  (** maximum total allocated space *)
  fsmblks : int;  (** space available in freed fastbin blocks *)
  uordblks : int;  (** total allocated space *)
  fordblks : int;  (** total free space *)
  keepcost : int;  (** top-most, releasable (via malloc_trim) space *)
} with sexp, bin_io

(** [mallinfo ()] @return information on the state of malloced memory
    (C-heap). *)
external mallinfo : unit -> mallinfo = "malloc_mallinfo_stub"

(** Malloc options *)
type opt =
  | TRIM_THRESHOLD  (** Maximum amount of unused top-most memory to keep
                        before releasing via malloc_trim in free(). *)
  | TOP_PAD  (** Amount of extra `padding' space to allocate or retain
                 whenever sbrk is called. *)
  | MMAP_THRESHOLD  (** Request size threshold for using mmap() to
                        service a request.  Requests of at least this size
                        that cannot be allocated using already-existing
                        space will be serviced via mmap. *)
  | MMAP_MAX  (** Maximum number of requests to simultaneously service
                  using mmap. *)
  | CHECK_ACTION  (** ??? *)
(*   | PERTURB  (** ??? *) *)
with sexp, bin_io

(** [mallopt opt n] sets malloc configuration option [opt] to [n]. *)
external mallopt : opt -> int -> unit = "malloc_mallopt_stub"

(** [malloc_trim n] release all but [n] bytes of freed top-most memory
    back to the system.

    @raise Failure if unsuccessful.
*)
external malloc_trim : int -> unit = "malloc_trim_stub"

(** [malloc_stats ()] prints brief summary statistics on stderr. *)
external malloc_stats : unit -> unit = "malloc_stats_stub"
