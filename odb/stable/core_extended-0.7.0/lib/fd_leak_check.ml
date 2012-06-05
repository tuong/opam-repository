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

let report_open_files_num num_open_fds =
  eprintf "Running emergency file descriptor dump:\n%!";
  for fd = 0 to num_open_fds do
    try
      let target = Unix.readlink (sprintf "/proc/self/fd/%d" fd) in
      eprintf "fd %d -> %s\n%!" fd target
    with _ -> ()
  done;
  if Sys.file_exists_exn "/usr/sbin/lsof" then begin
    eprintf "Also running lsof file descriptor dump:\n%!";
    let pid = Unix.fork () in
    if pid = 0 then Signal.send_i ~pid:(Unix.getpid ()) Signal.stop
    else begin
      for fd = 3 to num_open_fds - 3 do
        try Unix.close (Obj.magic fd : Unix.file_descr)
        with _ -> ()
      done;
      Unix.sleep 1;
      ignore (Sys.command (Printf.sprintf "/usr/sbin/lsof -p %d 1>&2" pid));
      Signal.send_i ~pid Signal.cont;
      Unix.exit_immediately 0
    end
  end

let report_open_files () =
  report_open_files_num (Unix.get_num_open_fds ())

let report_on_exn exn =
  let module U = Unix in
  match exn with
  | U.Unix_error ((U.EMFILE | U.ENFILE), _, _) -> report_open_files ()
  | _ -> ()

let run_check_at_exit = ref true
let critical = ref 0.9

let max_fds () =
  let module R = Unix.RLimit in
  match (R.get `Num_file_descriptors).R.cur with
  | R.Infinity -> max_int
  | R.Limit n ->
    match Int.of_int64 n with
    | Some n -> n
    | None -> max_int
;;

let check_fd_leak () =
  if !run_check_at_exit then
    try
      let max_fds = max_fds () in
      let thresh = Float.round_towards_zero_exn (!critical *. float max_fds) in
      let num_open_fds = Unix.get_num_open_fds () in
      if num_open_fds > thresh then begin
        eprintf
          "at_exit: too many open files: have %d, critical %d, maximum %d\n%!"
          num_open_fds thresh max_fds;
        report_open_files_num num_open_fds
      end
    with exn ->
      eprintf "exception checking for file descriptor leak: %s\n%!"
        (Exn.to_string exn)


let () = at_exit check_fd_leak

let percent_fds_in_use () =
  float (Unix.get_num_open_fds ()) /. float (max_fds ())
;;
