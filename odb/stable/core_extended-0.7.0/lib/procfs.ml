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

(* Learn more about this business by consulting proc(5) *)

type bigint = Big_int.big_int

module Inode = struct
  type t = Int64.t
  let of_string = Int64.of_string
  let to_string = Int64.to_string
end

let procdir = "/proc"

type stat =
  {
    comm:        string;
    state:       char;
    ppid:        int;
    pgrp:        int;
    session:     int;
    tty_nr:      int;
    tpgid:       int;
    flags:       bigint;
    minflt:      bigint;
    cminflt:     bigint;
    majflt:      bigint;
    cmajflt:     bigint;
    utime:       bigint;
    stime:       bigint;
    cutime:      bigint;
    cstime:      bigint;
    priority:    bigint;
    nice:        bigint;
    unused:      bigint;
    itrealvalue: bigint;
    starttime:   bigint;
    vsize:       bigint;
    rss:         bigint;
    rlim:        bigint;
    startcode:   bigint;
    endcode:     bigint;
    startstack:  bigint;
    kstkesp:     bigint;
    signal:      bigint;
    blocked:     bigint;
    sigignore:   bigint;
    sigcatch:    bigint;
    wchan:       bigint;
    nswap:       bigint;
    cnswap:      bigint;
    exit_signal: int;
    processor:   int;
    rt_priority: bigint;
    policy:      bigint;
  }
;;

type statm =
  {
    size:     bigint;
    resident: bigint;
    share:    bigint;
    text:     bigint;
    lib:      bigint;
    data:     bigint;
    dt:       bigint;
  }
;;

type status =
  {
    uid:   int;
    euid:  int;
    suid:  int;
    fsuid: int;
    gid:   int;
    egid:  int;
    sgid:  int;
    fsgid: int;
  }
;;

type fd_stat =
  | Path of string
  | Socket of Inode.t
  | Pipe of Inode.t
  | Inotify
;;

type fd =
  {
    fd: int;
    fd_stat: fd_stat;
  }
;;

type process =
  {
    pid:         int;
    cmdline:     string;
    cwd:         string option;
    environ:     string option;
    exe:         string option;
    root:        string option;
    stat:        stat;
    statm:       statm;
    status:      status;
    task_stats:  stat Int.Map.t;
    top_command: string;
    fds:         fd list option;
  }
;;

(* lu and ld match the proc(5) format strings %lu and %ld *)
let lu x = Big_int.big_int_of_string x
let ld x = lu x   (* bigint everything to make math easier *)

let load_proc_stat s =
  let extract_command s =
  (*
    * extract_cmdline, for a stat string such as:
    *   "14574 (cat) R 10615 14574 10615 34820 14574 4194304 164 0..."
    * returns this tuple
    *   "cat", "R 10615 14574 10615..."
    *)
    let i = String.index_exn s '(' in
    let j = String.rindex_exn s ')' in
    (String.sub s ~pos:(i+1) ~len:(j-(i+1)),
    String.sub s ~pos:(j+1) ~len:((String.length s)-(j+1)))
  in
  let comm, rest = extract_command s in
    let a = Array.of_list (String.split (String.strip rest) ~on:' ') in
    let d x = int_of_string x in
    let c x = x.[0] in
    {
      comm        = comm;
      state       = c a.(0);
      ppid        = d a.(1);
      pgrp        = d a.(2);
      session     = d a.(3);
      tty_nr      = d a.(4);
      tpgid       = d a.(5);
      flags       = ld a.(6);
      minflt      = ld a.(7);
      cminflt     = ld a.(8);
      majflt      = ld a.(9);
      cmajflt     = ld a.(10);
      utime       = ld a.(11);
      stime       = ld a.(12);
      cutime      = ld a.(13);
      cstime      = ld a.(14);
      priority    = ld a.(15);
      nice        = ld a.(16);
      unused      = ld a.(17);
      itrealvalue = ld a.(18);
      starttime   = lu a.(19);
      vsize       = lu a.(20);
      rss         = ld a.(21);
      rlim        = lu a.(22);
      startcode   = lu a.(23);
      endcode     = lu a.(24);
      startstack  = lu a.(25);
      kstkesp     = lu a.(26);
      signal      = lu a.(27);
      blocked     = lu a.(28);
      sigignore   = lu a.(29);
      sigcatch    = lu a.(30);
      wchan       = lu a.(31);
      nswap       = lu a.(32);
      cnswap      = lu a.(33);
      exit_signal = d a.(34);
      processor   = d a.(35);
      rt_priority = lu a.(36);
      policy      = lu a.(37);
    }

let load_proc_statm s =
  let a = Array.of_list (String.split s ~on:' ') in
  {
    size     = lu a.(0);
    resident = lu a.(1);
    share    = lu a.(2);
    text     = lu a.(3);
    lib      = lu a.(4);
    data     = lu a.(5);
    dt       = lu a.(6);
  }

let load_proc_status s =
  (* Splits "foo: 1\nbar: 2\n" into [Some ("foo"," 1"); Some ("bar"," 2"); None] *)
  let records = List.map (String.split s ~on:'\n')
    ~f:(fun x -> String.lsplit2 x ~on:':')
  in
  let _, uids = Option.value_exn
    (List.find_exn records
       ~f:(fun kv -> match kv with
           | Some ("Uid",_) -> true
           | _ -> false))
  in
  let _, gids = Option.value_exn
    (List.find_exn records
       ~f:(fun kv -> match kv with
           | Some ("Gid",_) -> true
           | _ -> false))
  in
  sscanf (String.concat ~sep:" " [String.strip uids; String.strip gids])
    "%d %d %d %d %d %d %d %d"
    (fun a b c d e f g h -> { uid = a; euid = b; suid = c; fsuid = d;
                              gid = e; egid = f; sgid = g; fsgid = h; })
;;

let string_of_file fn = read_wrap ~f:In_channel.input_all fn ;;

(** [load_proc procdir pid] loads process information from /<procdir>/<pid>/*
    into a process type.  This is similar to how ps and top get their
    information. *)
let load_proc procdir pid =
  let slurp f fn =
    try
      Some (f (sprintf "/%s/%d/%s" procdir pid fn))
    with
    | Sys_error _ -> None
    | Unix.Unix_error (Unix.EACCES, _, _) -> None
    | Unix.Unix_error (Unix.ENOENT, _, _) -> None
    | Unix.Unix_error (Unix.EINVAL, _, _) -> None
  in
  let slurp_file fn = slurp string_of_file fn in
  let slurp_link fn = slurp Unix.readlink fn in
  let slurp_dir fn = slurp Sys.readdir fn in

  let required x = Option.value_exn x in
  let cmdline = required (slurp_file "cmdline") in
(*
 * Process command name varies
 *
 * cmdline is ideal but not guaranteed to be there because the kernel
 *  - may discard it in lomem situations
 *  - discard it for zombie processes
 *  - put nothing useful there for kernel processes
 *
 * The exe symlink might be useful, it's the name of the executable
 * which started the process, but permission is usually denied for
 * non-root/non-self viewers.
 *
 * The stat.command field will ALWAYS be there but is truncated
 * to 16 chars; we do here what top does: use cmdline if it is
 * populated, otherwise use stat.command.
 *)
  let stat = load_proc_stat (required (slurp_file "stat")) in
  let top_command =
    (if cmdline = "" then
      stat.comm
    else
      String.tr ~target:'\x00' ~replacement:' ' cmdline)
    |! String.strip
  in
  let task_stats =
    Array.fold_right (required (slurp_dir "task"))
      ~init:Int.Map.empty
      ~f:(fun task m ->
        Int.Map.add m
          ~key:(int_of_string task)
          ~data:(load_proc_stat (required (slurp_file
            (String.concat ~sep:"/" ["task";task;"stat"])))))
  in
  let fds =
    try
      Some (
        Sys.readdir (sprintf "%s/%d/fd" procdir pid) |!
        Array.map ~f:int_of_string |!
        Array.filter_map
        ~f:(fun fd ->
          match slurp Unix.readlink (sprintf "fd/%d" fd) with
          | Some path ->
              let parse inode = (* "[123]" -> 123 *)
                inode |!
                String.chop_prefix_exn ~prefix:"[" |!
                String.chop_suffix_exn ~suffix:"]" |!
                Inode.of_string
              in
              Some {
                fd = fd;
                fd_stat =
                  match String.split ~on:':' path with
                  | "socket"::inode::[] -> Socket (parse inode)
                  | "pipe"::inode::[]   -> Pipe (parse inode)
                  | "inotify"::[]       -> Inotify
                  | _                   -> Path path;
              }
          | None -> None) |! Array.to_list)
    with
    | Sys_error _ -> None
  in
  {
    pid         = pid;
    cmdline     = cmdline;
    cwd         = slurp_link "cwd";
    environ     = slurp_file "environ";
    exe         = slurp_link "exe";
    root        = slurp_link "root";
    stat        = stat;
    statm       = load_proc_statm (required (slurp_file "statm"));
    status      = load_proc_status (required (slurp_file "status"));
    task_stats  = task_stats;
    top_command = top_command;
    fds         = fds;
  }
;;

let is_pid s =
  try
    let _ = int_of_string s in
    true
  with
    Failure (_) -> false
;;

let get_all_procs () =
  let procs = Sys.readdir procdir in
  let a = Array.filter_map
    ~f:(fun pid ->
          try
            Some (load_proc procdir (int_of_string pid))
          with
            (* Failures usually aren't a fatal *system* condition.
               procfs queries on Linux simply are not consistent.
               They're generally thwarted by terminating processes.
               We simply skip the proc entry on failure. *)
          | Not_found | Failure _ -> None) procs
  in
  Array.to_list a
;;

let with_pid_exn = load_proc procdir ;;

let with_pid pid = Option.try_with (fun () -> load_proc procdir pid) ;;

let with_uid uid =
  List.filter (get_all_procs ()) ~f:(fun p -> p.status.uid = uid)

let with_username_exn name = with_uid (Unix.Passwd.getbyname_exn name).Unix.Passwd.uid ;;

let with_username name = Option.try_with (fun () -> with_username_exn name) ;;

(*
 * This is a partial translation of
 *   sysinfo.c:init_Hertz_value from procps (top)
 *)
let hertz () =
  let get_uptime () =
    let uptime1 = string_of_file (procdir^"/uptime") in
    sscanf uptime1 "%f" (fun x -> x)
  in
  let rec sample () =
    let up1 = get_uptime () in

    let stat = string_of_file (procdir^"/stat") in
    let statlines = String.split stat ~on:'\n' in

    (* On modern systems the second line is always cpu0 (even uni-processors) *)
    let statline = Option.value_exn (List.nth statlines 1) in

    let user_j, nice_j, sys_j, idle_j, iowait_j =
      sscanf statline "cpu0 %Lu %Lu %Lu %Lu %Lu" (fun a b c d e -> a,b,c,d,e)
    in
    let up2 = get_uptime() in
    if ((up2 -. up1) > 0.01) then
      sample ()  (* sampling latency too high.  try again *)
    else
      let (+) = Int64.(+) in
      user_j + nice_j + sys_j + idle_j + iowait_j, ((up1 +. up2) /. 2.)
  in
  let jiffies, seconds = sample () in
  (Int64.to_float jiffies) /. seconds
;;

let memtotal () =
  let meminfo = string_of_file (procdir^"/meminfo") in
  let memlines = String.split meminfo ~on:'\n' in
  let total = Option.value_exn (List.hd memlines) in
  sscanf total "MemTotal: %d kB" (fun x -> x)
;;

let pgrep f = List.filter (get_all_procs ()) ~f ;;

let pkill ~signal f =
  List.fold (get_all_procs ()) ~init:[] ~f:(fun a p ->
      if not (f p) then a else
      let result =
        try Ok (ignore (Signal.send signal ~pid:p.pid)) with
        | Unix.Unix_error (e, _, _) -> Error e
        | e ->
            Exn.reraisef e
              "Procfs.pkill caught exception trying to signal process %d"
              p.pid ()
      in
      (p.pid, result) :: a)
;;
