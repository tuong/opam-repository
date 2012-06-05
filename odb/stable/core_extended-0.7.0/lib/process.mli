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

(**
   Low-level process handling

   This is low-level enough that you should probably be using [Shell] instead
   to dispatch processes.
*)

open Core.Std

module Status : sig
  type t =  [ `User_killed | `Timeout | Unix.Process_status.t ] with sexp_of

  val to_string : t -> string

end

module Command_result : sig
  type t= {
    status      : Status.t;
    stdout_tail : string;
    stderr_tail : string
  }
end

(** kills a process by sending [signal]; waiting for [wait_for] and then
    sending a [sigkill].
    You need to set is_child to true when killing child processes or run waitpid
    on them in another.
    @raises Failure if the target program hangs for more that [wait_for] after
    receiving the [sigkill].
*)
val kill :
  ?is_child:bool ->
  ?wait_for:Time.Span.t ->
  ?signal:Signal.t ->
  int
  -> unit

val run :
  ?timeout:Time.Span.t
  -> ?use_extra_path:bool
  -> ?working_dir:string
  -> ?env:([`Extend of (string * string) list
          | `Replace of (string * string) list])
  -> ?input:string
  -> ?stdoutf:(string -> int -> unit)
  -> ?stderrf:(string -> int -> unit)
  -> ?tail_len:int
  -> prog:string
  -> args:string list
  -> unit
  -> Command_result.t
(** [stdoutf] and [stderr] can raise [Exit] to indicate they are done

    @param stop this paramater is there as a hack for the compile daemon. Please
    avoid depending on it as it should go sometime in the near future
*)
