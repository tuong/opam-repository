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
open Format

type opt = PID | CONS | ODELAY | NDELAY | NOWAIT | PERROR

type fac =
  | KERN | USER | MAIL | DAEMON | AUTH | SYSLOG
  | LPR | NEWS | UUCP | CRON | AUTHPRIV | FTP
  | LOCAL0 | LOCAL1 | LOCAL2 | LOCAL3 | LOCAL4 | LOCAL5 | LOCAL6 | LOCAL7

type lev = EMERG | ALERT | CRIT | ERR | WARNING | NOTICE | INFO | DEBUG

let string_of_lev = function
  | EMERG -> "EMERG"
  | ALERT -> "ALERT"
  | CRIT -> "CRIT"
  | ERR -> "ERR"
  | WARNING -> "WARNING"
  | NOTICE -> "NOTICE"
  | INFO -> "INFO"
  | DEBUG -> "DEBUG"

let lev_of_string = function
  | "EMERG" -> EMERG
  | "ALERT" -> ALERT
  | "CRIT" -> CRIT
  | "ERR" -> ERR
  | "WARNING" -> WARNING
  | "NOTICE" -> NOTICE
  | "INFO" -> INFO
  | "DEBUG" -> DEBUG
  | _ -> failwith "lev_of_string: unknown level"

let string_of_fac = function
  | KERN -> "KERN"
  | USER -> "USER"
  | MAIL -> "MAIL"
  | DAEMON -> "DAEMON"
  | AUTH -> "AUTH"
  | SYSLOG -> "SYSLOG"
  | LPR -> "LPR"
  | NEWS -> "NEWS"
  | UUCP -> "UUCP"
  | CRON -> "CRON"
  | AUTHPRIV -> "AUTHPRIV"
  | FTP -> "FTP"
  | LOCAL0 -> "LOCAL0"
  | LOCAL1 -> "LOCAL1"
  | LOCAL2 -> "LOCAL2"
  | LOCAL3 -> "LOCAL3"
  | LOCAL4 -> "LOCAL4"
  | LOCAL5 -> "LOCAL5"
  | LOCAL6 -> "LOCAL6"
  | LOCAL7 -> "LOCAL7"

let all_levs = [| DEBUG; INFO; NOTICE; WARNING; ERR; CRIT; ALERT; EMERG |]

let all_str_levs =
  [| "DEBUG"; "INFO"; "NOTICE"; "WARNING"; "ERR"; "CRIT"; "ALERT"; "EMERG" |]

let array_el_ix ar el =
  let rec loop ix =
    if ar.(ix) = el then ix
    else if ix = 0 then raise Not_found
    else loop (ix - 1) in
  loop (Array.length ar - 1)

let compare_lev lev1 lev2 =
  array_el_ix all_levs lev1 - array_el_ix all_levs lev2

external int_of_opt : opt -> int = "int_of_opt_stub"
external int_of_fac : fac -> int = "int_of_fac_stub"
external int_of_lev : lev -> int = "int_of_lev_stub"
external openlog : string -> int -> int -> unit = "openlog_stub"
external syslog : int -> string -> unit = "syslog_stub"
external closelog : unit -> unit = "closelog_stub" "noalloc"
external setlogmask : int -> unit = "setlogmask_stub" "noalloc"

let coll_opts opts opt = int_of_opt opt lor opts
let int_of_opts lst = List.fold ~f:coll_opts ~init:0 lst

let coll_levs levs lev = int_of_lev lev lor levs
let int_of_levs lst = List.fold ~f:coll_levs ~init:0 lst

let fac_ref = ref USER

let openlog ?(id = Sys.argv.(0)) ?(opt = []) ?(fac = USER) () =
  fac_ref := fac;
  openlog id (int_of_opts opt) (int_of_fac fac)

let syslog ?(fac = !fac_ref) ?(lev = INFO) msg =
  syslog (int_of_lev lev lor int_of_fac fac) msg

let syslog_printf ?fac ?lev fmt = ksprintf (syslog ?fac ?lev) fmt

let esyslog ?(fac = !fac_ref) ?(lev = INFO) msg =
  syslog ~fac ~lev msg;
  eprintf "%s/%s: %s\n" (string_of_fac fac) (string_of_lev lev) msg;
  flush stderr

let esyslog_printf ?fac ?lev fmt = ksprintf (esyslog ?fac ?lev) fmt

let logmask_range ?(to_lev = EMERG) from_lev =
  let ix_to = array_el_ix all_levs to_lev in
  let ix_from = array_el_ix all_levs from_lev in
  let logmask = ref 0 in
  for i = ix_to to ix_from do
    logmask := int_of_lev all_levs.(i) lor !logmask
  done;
  !logmask

let setlogmask ?(levs = []) ?(from_lev = DEBUG) ?to_lev () =
  setlogmask (int_of_levs levs lor logmask_range ?to_lev from_lev)
