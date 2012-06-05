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





open Std_internal

module Range : sig
  (* A range represents a closed interval of integers *)
  type t = private { lo: int; hi: int } (* Invariant: lo <= hi *)

  (** Create [t] from a range *)
  val make : int -> int -> t

  val to_string : t -> string

  (** [compare s t] returns:
      `Mergeable : [s] and [t] have a intersection,
                   or they are adjacent: ex. [10..12] and [13..15]
      `Lt_and_not_adjacent : if [s] is less than [t] but not adjacent
      `Gt_and_not_adjacent : if [s] is greater than [t] but not adjacent
  *)
  val compare : t -> t -> [ `Lt_and_not_adjacent
                          | `Gt_and_not_adjacent
                          | `Mergeable ]

  (** [merge s t] merges mergeable ranges *)
  val merge : t -> t -> [ `Ok of t
                        | `Lt_and_not_adjacent
                        | `Gt_and_not_adjacent ]

  val contains : int -> t -> bool
end = struct
  type t = { lo: int; hi: int }

  let make x y = if x <= y then { lo = x; hi = y } else { lo = y; hi = x }

  let to_string t = sprintf "(%d, %d)" t.lo t.hi

  (* on the number line, r1 is either on the left, on the right, or
     intersected with r2 *)
  let compare r1 r2 =
    if r1.hi < r2.lo - 1 then `Lt_and_not_adjacent
    else if r1.lo > r2.hi + 1 then `Gt_and_not_adjacent
    else `Mergeable

  let merge r1 r2 =
    match compare r1 r2 with
    | `Lt_and_not_adjacent -> `Lt_and_not_adjacent
    | `Gt_and_not_adjacent -> `Gt_and_not_adjacent
    | `Mergeable -> `Ok {lo = Int.min r1.lo r2.lo; hi = Int.max r1.hi r2.hi}

  let contains i r = r.lo <= i && i <= r.hi
end

type t = Range.t list

(* invariant : the elements of [t] must be discrete (not mergeable) each other and sorted
   in DECREASING order. *)

let create () = []

let to_string t = String.concat ~sep:", " (List.rev_map t ~f:Range.to_string)

let add_range t x y =
  (* note: not tail recursive *)
  let rec loop ranges to_add =
    match ranges with
    | r :: rest ->
        (* the following keeps the invariant: discrete + sorted *)
        begin match Range.merge to_add r with
        | `Lt_and_not_adjacent -> r :: loop rest to_add
        | `Gt_and_not_adjacent -> to_add :: r :: rest
        | `Ok merged -> loop rest merged
        end
    | [] -> [to_add]
  in
  loop t (Range.make x y)

let add t i = add_range t i i

let mem t i = List.exists t ~f:(fun x -> Range.contains i x)

let ranges t = List.map t ~f:(fun {Range.lo = lo; hi = hi} -> (lo, hi))

let max t =
  match t with
  | [] -> None
  | {Range.hi=max} :: _ -> Some max

let min t =
  match List.last t with
  | None -> None
  | Some { Range.lo=min } -> Some min
