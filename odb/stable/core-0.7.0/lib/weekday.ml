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

module Array = Core_array
module Int = Core_int
module String = Core_string
module Hashtbl = Core_hashtbl

let failwithf = Core_printf.failwithf

let num_days = 7

type t = int

let invariant t =
  assert (0 <= t && t < num_days);
;;

let of_int i =
  if 0 <= i && i < num_days then
    Some i
  else
    None
;;

let of_int_exn i =
  if 0 <= i && i < num_days then
    i
  else
    failwithf "Weekday.of_int_exn %d" i ()
;;

let to_int t = t

let sun = 0
let mon = 1
let tue = 2
let wed = 3
let thu = 4
let fri = 5
let sat = 6

type variant = [ `Sun | `Mon | `Tue | `Wed | `Thu | `Fri | `Sat ]

type rep = {
  string : string;
  t : t;
  variant : variant;
}

let reps =
  Array.map
    [|("SUN", sun, `Sun);
      ("MON", mon, `Mon);
      ("TUE", tue, `Tue);
      ("WED", wed, `Wed);
      ("THU", thu, `Thu);
      ("FRI", fri, `Fri);
      ("SAT", sat, `Sat)|]
    ~f:(fun (string, t, variant) ->
      { string = string; t = t; variant = variant; })
;;

type stringable = t

let to_string t = reps.(t).string

let of_string =
  let table =
    String.Table.of_alist_exn
      (Array.to_list (Array.map reps ~f:(fun r -> (r.string, r.t))))
  in
  fun str ->
    match Hashtbl.find table (String.uppercase str) with
    | None -> failwithf "Invalid weekday: %s" str ()
    | Some x -> x
;;

include Sexpable.Of_stringable (struct
  type stringable = t
  let of_string = of_string
  let to_string = to_string
end)

let get t = reps.(t).variant

let create = function
  | `Sun -> 0
  | `Mon -> 1
  | `Tue -> 2
  | `Wed -> 3
  | `Thu -> 4
  | `Fri -> 5
  | `Sat -> 6
;;

let shift t i = Int.Infix.( % ) (t + i) num_days

let is_sun_or_sat t =
  t = sun || t = sat

include (Int : sig
  include Binable.S with type binable = t
  include Hashable.S with type hashable = t
  include Comparable.S with type comparable = t
end)
