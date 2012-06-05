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
open Core_extended.Std
open OUnit

module Lru = Cache.Lru

let seteq l1 l2 =
  List.sort ~cmp:compare l1 = List.sort ~cmp:compare l2

let str l =
  "[" ^ String.concat ~sep:", " (List.map ~f:string_of_int l) ^ "]"

let dead = ref []
let clear_dead () = dead := []
let lru = Lru.create ~destruct:(Some (fun v -> dead := v::!dead)) 3

let touch v = Lru.add lru ~key:v ~data:v

let test = "lru" >::: [
  "1234-1" >:: (fun () -> clear_dead ();
    touch 1; touch 2; touch 3; touch 4;
    (str !dead) @? seteq !dead [1]);
  "235-4" >:: (fun () -> clear_dead ();
    touch 2; touch 3; touch 5;
    (str !dead) @? seteq !dead [4]);
  "36-2" >:: (fun () -> clear_dead ();
    touch 3; touch 6;
    (str !dead) @? seteq !dead [2]);
  "7-5" >:: (fun () -> clear_dead ();
    touch 7;
    (str !dead) @? seteq !dead [5]);
  "c-763" >:: (fun () ->
    clear_dead ();
    Lru.clear lru;
    (str !dead) @? seteq !dead [7;6;3]);
  "890-" >:: (fun () ->
    clear_dead ();
    touch 8; touch 9; touch 0;
    (str !dead) @? seteq !dead []);
]
