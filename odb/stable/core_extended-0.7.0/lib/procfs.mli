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

open Core.Std

(**
   Process and system stats
*)

type bigint = Big_int.big_int

module Inode : sig
  type t
  val of_string : string -> t
  val to_string : t -> string
end

type stat =
  {
    comm:        string; (** The filename of the executable *)
    state:       char;   (** One  character from the string "RSDZTW" *)
    ppid:        int;    (** The PID of the parent. *)
    pgrp:        int;    (** The process group ID of the process. *)
    session:     int;    (** The session ID of the process. *)
    tty_nr:      int;    (** The tty the process uses. *)
    tpgid:       int;    (** The process group ID of the process which
                            currently owns the tty... *)
    flags:       bigint; (** The kernel flags word of the process. *)
    minflt:      bigint; (** The number of minor faults the process has
                            made which have not required loading a memory
                            page from disk. *)
    cminflt:     bigint; (** The number of minor faults that the process’s
                            waited-for children have made. *)
    majflt:      bigint; (** The number of major faults the process has made
                            which have required loading a page from disk. *)
    cmajflt:     bigint; (** The number of major faults that the process’s
                            waited-for children have made. *)
    utime:       bigint; (** The number of jiffies that this process has been
                            scheduled in user mode. *)
    stime:       bigint; (** The number of jiffies that this process has been
                            scheduled in kernel mode. *)
    cutime:      bigint; (** The number of jiffies that this process’s waited-for
                            children have been scheduled in user mode. *)
    cstime:      bigint; (** The number of jiffies that this process’s waited-for
                            children have been scheduled in kernel mode. *)
    priority:    bigint; (** The standard nice value, plus fifteen.  The value
                            is never negative in the kernel. *)
    nice:        bigint; (** The nice value ranges from 19 to -19*)
    unused:      bigint; (** placeholder for removed field *)
    itrealvalue: bigint; (** The time in jiffies before the next SIGALRM is
                            sent to the process due to an interval timer. *)
    starttime:   bigint; (** The time in jiffies the process started after
                            system boot. *)
    vsize:       bigint; (** Virtual memory size in bytes. *)
    rss:         bigint; (** Resident Set Size: number of pages the process
                            has in real memory. *)
    rlim:        bigint; (** Current limit in bytes on the rss of the process. *)
    startcode:   bigint; (** The address above which program text can run. *)
    endcode:     bigint; (** The address below which program text can run. *)
    startstack:  bigint; (** The address of the start of the stack. *)
    kstkesp:     bigint; (** The current value of esp (stack pointer) *)
    signal:      bigint; (** The bitmap of pending signals. *)
    blocked:     bigint; (** The bitmap of blocked signals. *)
    sigignore:   bigint; (** The bitmap of ignored signals. *)
    sigcatch:    bigint; (** The bitmap of caught signals. *)
    wchan:       bigint; (** This  is  the "channel" in which the
                            process is waiting. Address of a system call. *)
    nswap:       bigint; (** (no longer maintained) *)
    cnswap:      bigint; (** (no longer maintained) *)
    exit_signal: int;    (** Signal sent to parent when we die. *)
    processor:   int;    (** CPU number last executed on. *)
    rt_priority: bigint; (** Real-time scheduling priority. *)
    policy:      bigint; (** Scheduling policy *)
  }

type statm =
  {
    size:     bigint; (** total program size *)
    resident: bigint; (** resident set size *)
    share:    bigint; (** shared pages *)
    text:     bigint; (** text (code) *)
    lib:      bigint; (** library *)
    data:     bigint; (** data/stack *)
    dt:       bigint; (** dirty pages (unused) *)
  }

type status =
  {
    uid:   int; (** Real user ID *)
    euid:  int; (** Effective user ID *)
    suid:  int; (** Saved user ID *)
    fsuid: int; (** FS user ID *)
    gid:   int; (** Real group ID *)
    egid:  int; (** Effective group ID *)
    sgid:  int; (** Saved group ID *)
    fsgid: int; (** FS group ID *)
  }

type fd_stat =
 | Path of string
 | Socket of Inode.t
 | Pipe of Inode.t
 | Inotify

type fd = {
  fd: int;          (** File descriptor (0=stdin, 1=stdout, etc.) *)
  fd_stat: fd_stat; (** Kind of file *)
}

type process =
  {
    pid:         int;            (** Process ID *)
    cmdline:     string;         (** Command-line (not reliable). *)
    cwd:         string option;  (** Symlink to working directory. *)
    environ:     string option;  (** Process environment. *)
    exe:         string option;  (** Symlink to executed command. *)
    root:        string option;  (** Per-process root (e.g. chroot) *)
    stat:        stat;           (** Status information. *)
    statm:       statm;          (** Memory status information. *)
    status:      status;         (** Some more assorted status information. *)
    task_stats:  stat Int.Map.t; (** Status information for each task (thread) *)
    top_command: string;         (** Show what top would show for COMMAND *)
    fds:         fd list option; (** File descriptors *)
  }

(** [get_all_procs] returns a list of all processes on the system *)
val get_all_procs : unit -> process list

(** [with_pid_exn pid] returns a single process that matches pid, or raises Not_found *)
val with_pid_exn : int -> process

(** [with_pid pid] returns a single process that matches pid *)
val with_pid : int -> process option

(** [with_uid uid] returns all processes owned by uid *)
val with_uid : int -> process list

(** [pgrep f] returns all processes for which f is true *)
val pgrep : (process -> bool) -> process list

(** [pkill ~signal f] sends the signal to all processes for which f returns true. It
   returns the list of processes that were signaled, and the resulting errors if any. *)
val pkill : signal:Signal.t -> (process -> bool) -> (int * (unit, Unix.error) Result.t) list

(** [with_username_exn user] calls with_uid after looking up the user's uid *)
val with_username_exn : string -> process list

(** [with_username user] calls with_uid after looking up the user's uid *)
val with_username : string -> process list option

(** [hertz] is the number of jiffies per second *)
val hertz : unit -> float

(** [memtotal] is the amount of physical memory in the system, in kB *)
val memtotal : unit -> int
