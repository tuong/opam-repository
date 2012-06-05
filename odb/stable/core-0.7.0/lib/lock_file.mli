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

(** Mutual exclusion between processes using flock and lockf.  A file is
    considered locked if either of these mechanisms works.

    These locks are OS-level but are Local (will not work across computers
    even if they mount the same directory).
*)

(** [create ?message path] tries to create a file at [path] containing the text
    [message], which defaults to the pid of the locking process.  It returns
    true on success, false on failure.  Note: there is no way to release the
    lock or the fd created inside!  It will only be released when the process
    dies.

    Note the lock file is not cleaned up for you when the process exits.
    Consider setting up an at_exit handler to unlink the lock-file on
    program termination.
 *)
val create : ?message : string -> string -> bool

(** [create_exn ?message path] is like [create] except that it throws
    an exception on failure instead of returning a boolean value *)
val create_exn : ?message : string -> string -> unit

(** [blocking_create t] tries to create the lock. If another process holds
    the lock this function will wait until it is released. *)
val blocking_create : ?message : string -> string -> unit

(** [is_locked path] returns true when the file at [path] exists and
    is locked, false otherwise. *)
val is_locked : string -> bool

(** an implementation neutral NFS lock file scheme that relies on the
    atomicity of link and rename over NFS (see NFS Illustrated, atomicity for
    more information).  There are a few caveats compared to local file locks:

    - These calls require the locker to have write access to the directory
      containing the file being locked.

    - Unlike a normal flock call the lock will not be removed when the calling
      program exits, so unlock must be called.

    - There is no protection provided to prevent a different caller
      from calling unlock.
*)
module Nfs : sig
  (** [lock ?message path] tries to lock the file at [path] by creating two new
      files [path].nfs_lock and [path].nfs_lock.msg.  [path].nfs_lock will be
      a hard link to [path] and [path].nfs_lock.msg will contain [message]
      (caller's hostname and pid by default).  This lock WILL NOT be released
      when the calling program exits.  You MUST call unlock. *)
  val create : ?message : string -> string -> bool

  (** [lock_exn ?message path] like lock, but throws an exception when it fails
      to obtain the lock *)
  val create_exn : ?message : string -> string -> unit

  (** [lock_blocking ?message path] like lock, but sleeps for 1 second between
      lock attempts and does not return until it succeeds *)
  val blocking_create : ?message : string -> string -> unit

  (** [unlock path] unlocks a file locked by some version of lock.  There is no
      protection provided to stop you from unlocking a file you have not
      locked *)
  val unlock : string -> unit

  (** [critical_section ?message path ~f] wrap function [f] (including
      exceptions escaping it) by first locking (using {!create_exn}) and
      then unlocking the given lock file. *)
  val critical_section : ?message : string -> string -> f : (unit -> 'a) -> 'a
end
