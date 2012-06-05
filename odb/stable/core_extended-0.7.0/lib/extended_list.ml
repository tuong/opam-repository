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

TYPE_CONV_PATH "Core_extended.Extended_list"
open Core.Std




let set_diff l1 l2 =
  let set = Set.of_list l2 in
  List.filter l1 ~f:(fun x -> not (Set.mem set x))

let set_inter l1 l2 =
  let set = Set.of_list l2 in
  List.dedup (List.filter l1 ~f:(fun x -> Set.mem set x))


let classify ?(equal=( = )) ~f list =
  let classify_element class_members_assoc this_member =
    let this_class = f this_member in
    let rec add_class_member new_class_members_assoc old_class_members_assoc =
      match old_class_members_assoc with
      | [] ->
          (this_class,[this_member])::new_class_members_assoc
      | (classs,members)::rest when equal classs this_class ->
          (classs, this_member::members)::new_class_members_assoc@rest
      | l::ls ->
          add_class_member (l::new_class_members_assoc) ls
    in
    add_class_member [] class_members_assoc
  in
  List.fold list
    ~init:[] ~f:classify_element

let take_while xs f =
  let rec loop xs rev_prefix = match xs with
    | x::xs' -> if f x then loop xs' (x::rev_prefix) else List.rev rev_prefix
    | []     -> List.rev rev_prefix
  in
  loop xs []

let split_while xs p =
        let rec loop acc = function
                | [] -> (List.rev acc, [])
                | x::xs as x_xs -> if p x then loop (x::acc) xs else (List.rev acc, x_xs)
        in loop [] xs

let intersperse t sep =
  match t with
  | [] -> []
  | x :: xs ->
      let rec loop res xs =
        match xs with
        | [] -> List.rev res
        | x :: xs ->
            loop (x :: sep :: res) xs
      in
      x :: loop [] xs
;;

let lcs = Extended_list__LCS.lcs
let number = Extended_list__multimerge.number

let multimerge = Extended_list__multimerge.multimerge
let multimerge_unique = Extended_list__multimerge.multimerge_unique

let square_unique ?null l =
  let headers = List.map ~f:(List.map ~f:fst) l in
  let header = multimerge_unique headers in
  let find_col_value =
    match null with
    | None ->
      fun row col_header -> List.Assoc.find_exn row col_header
    | Some default ->
      fun row col_header ->
        Option.value (List.Assoc.find row col_header) ~default
  in
  let body =
    List.map l
      ~f:(fun row ->
        List.map header
          ~f:(fun col_header -> find_col_value row col_header))
  in
  header,body

let square ?null l =
  let numbered =
    List.map l
      ~f:(fun row ->
        let row_header,row_values = List.split row in
        List.combine_exn (number row_header) row_values)
  in
  let header,body = square_unique ?null numbered in
  List.map ~f:fst header,body


let enumerate_from =
        let rec loop acc n = function
                | [] -> List.rev acc
                | x::xs -> loop ((x,n)::acc) (n+1) xs
        in
        fun xs -> loop [] xs

let fold_left_term lst ~f ~init =
  let rec loop lst ~f ~acc =
    match lst with
    | [] -> acc
    | hd :: tl ->
        match f acc hd with
        | `Final v -> v
        | `Continue acc -> loop tl ~f ~acc
  in
  loop lst ~f ~acc:init

let max ?(cmp=Pervasives.compare) l =
  List.reduce l
    ~f:(fun x y -> if cmp x y > 0 then x else y)

let min ?(cmp=Pervasives.compare) l =
  List.reduce l
    ~f:(fun x y -> if cmp x y < 0 then x else y)

let max_exn ?(cmp=Pervasives.compare) l =
  List.reduce_exn l
    ~f:(fun x y -> if cmp x y > 0 then x else y)

let min_exn ?(cmp=Pervasives.compare) l =
  List.reduce_exn l
    ~f:(fun x y -> if cmp x y < 0 then x else y)

let sort_and_group ~cmp l =
  List.group (List.sort l ~cmp) ~break:(fun x y -> cmp x y <> 0)

let equal =
  let rec loop ~equal xs ys =
    match xs with
    | [] ->
      begin match ys with
      | [] -> true
      | _ :: _ -> false
      end
    | x :: xs ->
      begin match ys with
      | [] -> false
      | y :: ys -> equal x y && loop ~equal xs ys
      end
  in
  loop
;;

let rec compare ~cmp t1 t2 =
  match t1 with
  | [] ->
    begin match t2 with
    | [] -> 0
    | _ :: _ -> -1
    end
  | x :: xs ->
    begin match t2 with
    | [] -> 1
    | y :: ys ->
      match cmp x y with
      | 0 -> compare ~cmp xs ys
      | res -> res
    end
;;



(* input: list of rows --> output: list of columns *)
let transpose =
  let rec transpose_aux t rev_columns =
    match List.partition_map t ~f:(function [] -> `Snd () | x :: xs -> `Fst (x, xs)) with
    | (_ :: _, _ :: _) -> None
    | ([], _) -> Some (List.rev_append rev_columns [])
    | (heads_and_tails, []) ->
      let (column, trimmed_rows) = List.split heads_and_tails in
      transpose_aux trimmed_rows (column :: rev_columns)
  in
  fun t ->
    transpose_aux t []

exception Transpose_got_lists_of_different_lengths of int list with sexp

let transpose_exn l =
  match transpose l with
  | Some l -> l
  | None ->
    raise (Transpose_got_lists_of_different_lengths (List.map l ~f:List.length))
;;

