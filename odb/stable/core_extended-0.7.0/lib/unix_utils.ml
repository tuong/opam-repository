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

module RLimit = Unix.RLimit

(* Handling RAM limits *)

let physical_ram () =
  Int64.( * ) (Unix.sysconf Unix.PAGESIZE) (Unix.sysconf Unix.PHYS_PAGES)

type ram_usage_limit = Unlimited | Absolute of int64 | Relative of float

let set_ram_limit l =
  RLimit.set `Virtual_memory
    {
      RLimit.cur = RLimit.Limit l;
      RLimit.max = RLimit.Infinity;
    }

let apply_ram_usage_limit = function
  | Unlimited -> ()
  | Absolute i -> set_ram_limit i
  | Relative f ->
      set_ram_limit (Int64.of_float (f *. Int64.to_float (physical_ram ())))

let ram_msg =
  "RAM limit should be either an integer or a float between 0 and 1, not "

let string_to_ram_usage_limit s =
  try
    let i = Int64.of_string s in
    if i > Int64.zero then Absolute i
    else Unlimited
  with Failure _ ->
    let f = float_of_string s in
    if f < 0. || f > 1. then raise (Arg.Bad (ram_msg ^ s));
    if f > 0. then Relative f
    else Unlimited

let ram_limit_spec =
  (
    "-ram_limit",
    Arg.String (fun s -> apply_ram_usage_limit (string_to_ram_usage_limit s)),
    "num Limit RAM consumption either as an absolute number of bytes or as \
     a fraction of the total RAM, 0 - no limit"
  )


(* Signal handling *)

let all_sigs =
  [
    Signal.abrt;
    Signal.alrm;
    Signal.fpe;
    Signal.hup;
    Signal.ill;
    Signal.int;
    Signal.kill;
    Signal.pipe;
    Signal.quit;
    Signal.segv;
    Signal.term;
    Signal.usr1;
    Signal.usr2;
    Signal.chld;
    Signal.cont;
    Signal.stop;
    Signal.tstp;
    Signal.ttin;
    Signal.ttou;
    Signal.vtalrm;
    Signal.prof;
  ]


let wrap_block_signals f =
  let blocked_sigs = Signal.sigprocmask `Set all_sigs in
  protect ~f ~finally:(fun () ->
    ignore (Signal.sigprocmask `Set blocked_sigs))



(* at_exit functions are honored only when terminating by exit, not by signals,
   so we need to do some tricks to get it run by signals too.
   NB: Ctrl-C is _not_ handled by this function, i.e., it terminates a program
   without running at_exit functions. *)
let ensure_at_exit () =
  let pid = Unix.getpid () in
  let handler signal =
    do_at_exit ();
    Signal.set signal `Default;
    Signal.send_i signal ~pid in
  List.iter ~f:(fun s -> Signal.set s (`Handle handler)) [
    (* there are the signals which terminate a program due to
       "external circumstances" as opposed to "internal bugs" *)
    Signal.hup;
    Signal.quit;
    Signal.term;
  ]
