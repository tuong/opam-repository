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

module Hashtbl = Core_hashtbl
module Array = Core_array
module Int = Core_int
module List = Core_list
module Sexp = Core_sexp
module String = Core_string

let failwithf = Core_printf.failwithf

let num_months = 12

type t = int

let invariant t =
  assert (0 <= t && t < num_months);
;;

let is_valid_month i = 1 <= i && i <= num_months

let of_int i =
  if is_valid_month i then Some (i - 1)
  else None
;;

let of_int_exn i =
  if is_valid_month i then i - 1
  else failwithf "Month.of_int_exn %d" i ()
;;

let to_int t = t + 1

let shift t i = Int.Infix.( % ) (t + i) num_months

let jan = 0
let feb = 1
let mar = 2
let apr = 3
let may = 4
let jun = 5
let jul = 6
let aug = 7
let sep = 8
let oct = 9
let nov = 10
let dec = 11

let all = List.init num_months ~f:Fn.id

type variant = [ `Jan | `Feb | `Mar | `Apr | `May | `Jun
               | `Jul | `Aug | `Sep | `Oct | `Nov | `Dec ]
with sexp_of

type stringable = t

let create = function
  | `Jan -> jan
  | `Feb -> feb
  | `Mar -> mar
  | `Apr -> apr
  | `May -> may
  | `Jun -> jun
  | `Jul -> jul
  | `Aug -> aug
  | `Sep -> sep
  | `Oct -> oct
  | `Nov -> nov
  | `Dec -> dec
;;

let all_variants =
  [| `Jan; `Feb; `Mar; `Apr; `May; `Jun; `Jul; `Aug; `Sep; `Oct; `Nov; `Dec |]
;;

let all_strings =
  Array.map all_variants ~f:(fun variant ->
    Sexp.to_string (sexp_of_variant variant))
;;

let get t = all_variants.(t)

let to_string t = all_strings.(t)

let of_string =
  let module T = String.Table in
  let table = T.create ~size:num_months () in
  Array.iteri all_strings ~f:(fun t s ->
    Hashtbl.replace table ~key:(String.uppercase s) ~data:t);
  fun str ->
    
    match Hashtbl.find table (String.uppercase str) with
    | None -> failwithf "Invalid month: %s" str ()
    | Some x -> x
;;

let sexp_of_t_mode = ref `Upper

include Sexpable.Of_stringable (struct
  type stringable = t
  let of_string = of_string
  let to_string t =
    let s = to_string t in
    match !sexp_of_t_mode with
    | `Upper -> s
    | `Lower -> String.lowercase s
    | `Capitalized -> String.capitalize s
end)

include (Int : sig
  include Binable.S with type binable = t
  include Comparable.S with type comparable = t
  include Hashable.S with type hashable = t
end)
