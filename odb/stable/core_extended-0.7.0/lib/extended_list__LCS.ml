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

(* LCS.... *)
let lcsTBL x y m n =
  let c = Array.make_matrix ~dimx:(m+1) ~dimy:(n+1) 0 in
  for i = 0 to m-1 do
    for j = 0 to n -1 do
      if x.(i) = y.(j) then
        c.(i+1).(j+1) <- c.(i).(j) + 1
      else
        c.(i+1).(j+1) <- max c.(i+1).(j) c.(i).(j+1)
    done
  done;
  c

let rec lcsBacktrace c x y i j acc =
  if i=0 || j=0 then
    acc
  else if x.(i-1) = y.(j-1) then
    lcsBacktrace c x y (i-1) (j-1) (x.(i-1)::acc)
  else if c.(i).(j-1) > c.(i-1).(j) then
    lcsBacktrace c x y i (j-1) acc
  else
    lcsBacktrace c x y (i-1) j acc

(** Naive dynamic programming LCS *)
let lcs_kernel x y =
  let m = Array.length x
  and n = Array.length y in
  let c = lcsTBL x y m n in
  lcsBacktrace c x y m n []

(** Find common front part for an LCS *)
let rec common_start x y acc =
  match x,y with
  | h::t,h'::t' when h = h' -> common_start t t' (h::acc)
  | _ -> acc,x,y

(** LCS with common front and back part detection optimization.*)
let lcs x y =
  let rev_start,x,y = common_start x y [] in
  let stop,rev_x,rev_y = common_start (List.rev x) (List.rev y) [] in
  let lcs_middle = lcs_kernel (Array.of_list rev_x) (Array.of_list rev_y) in
  List.rev_append rev_start (List.rev_append lcs_middle stop)
