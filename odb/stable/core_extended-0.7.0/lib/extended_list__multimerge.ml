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
let rec count__loop cnt el = function
  | [] -> cnt
  | h::t when h = el -> count__loop (cnt + 1) el t
  | _::t -> count__loop cnt el t

(*
  [ count l el]
  Counts the occurences of [el] in [l]
*)
let count (l:'a list) (el:'a) : int = count__loop 0 el l

let rec number__loop seen acc = function
  | [] -> List.rev acc
  | h::t -> number__loop (h::seen) ((h,count seen h)::acc) t

let number (l:'a list) : ('a * int) list = number__loop [] [] l



let unnumber = List.map ~f:fst


type 'node graph = ('node * 'node list) list

let insert_edge (graph:'a graph) (node:'a) (child:'a) : 'a graph =
  let rec loop acc = function
    | [] -> (node,[child])::graph
    | (node',children)::l when node' = node ->
        if List.mem ~set:children child then
          graph
        else
          (node,(child::children))::(List.rev_append l acc)
    | h::t -> loop (h::acc) t
  in
  loop [] graph

let insert_node (graph:'a graph) (node:'a) : 'a graph =
  if List.Assoc.mem graph node then
    graph
  else
    (node,[]) :: graph

let children (graph:'a graph) (node:'a) : 'a list =
  List.Assoc.find_exn graph node

(** A topological sort that will degrade nicely in the presence of cycles. *)
let top_sort (graph:'a graph) : 'a list =
  let rec visit (dead,l) v =
    if List.mem v ~set:dead then
      (dead,l)
    else
      let dead,l =
        List.fold (children graph v)
          ~f:visit
          ~init:((v::dead),l)
      in
      dead,(v::l)
  in
  let _,l = List.fold graph
    ~f:(fun acc (node,_child) -> visit acc node)
    ~init:([],[])
  in
  l

let rec add_dep_list graph = function
  | []  -> graph
  | [node] -> insert_node graph node
  | node::((child::_) as l) -> add_dep_list (insert_edge graph node child) l

let multimerge_unique l =
  let graph = List.fold ~f:add_dep_list ~init:[] l in
  top_sort graph

let multimerge l =
  let l = List.map ~f:number l in
  unnumber (multimerge_unique l)
