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

open Core.Std
open OUnit

let () = Random.self_init ()
let esc_test i = int_of_string (Int.to_string_hum i) = i

let bound = 2 lsl 10

let rand () =
  let rec aux acc cnt =
    if cnt = 0 then
      acc
    else
      let bit = if Random.bool () then 1 else 0 in
      aux (2*acc + bit) (cnt -1)
  in
  let aval = aux 1 (Random.int (Sys.word_size - 3)) in
  if Random.bool () then
    - aval
  else
    aval
(* Random with a distribution favouring small ones*)

let test =
  "core_int" >:::
    [ "to_string_hum" >::
        (fun () ->
           "random" @? (
             List.init ~f:(fun _ -> rand ()) 10_000
           |! List.for_all ~f:esc_test
           );
           "max_int" @? esc_test max_int;
           "min_int" @? esc_test min_int
        )
    ]
