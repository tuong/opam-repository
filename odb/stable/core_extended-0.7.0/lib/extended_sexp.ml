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
open Sexplib.Sexp

let is_atom = function Atom _ -> true | List _ -> false
let is_list = function Atom _ -> false | List _ -> true

let list l = List l
let atom a = Atom a

let comment s =
  match (String.split s ~on:'\n') with
  | [] -> ""
  | h::l -> (String.concat ~sep:"\n;; " ((";; "^h)::l)) ^ "\n"

open Pp.Infix
let indent = 2

let rec pp_hum' fmt = function
  | Atom s -> Sexplib.Pre_sexp.pp_maybe_esc_str fmt s
  | List l when List.for_all ~f:is_atom l ->
    Format.pp_open_hovbox fmt 2;
    pp_hum_rest' fmt l
  | List l ->
      Format.pp_open_hvbox fmt 2;
      pp_hum_rest' fmt l
and pp_hum_rest' fmt l =
    Format.pp_print_string fmt "(";
    let rec loop = function
      | [] -> ()
      | [v] -> pp_hum' fmt v
      | h::t ->
          pp_hum' fmt h;
          Format.pp_print_space fmt ();
          loop t
    in
    loop l;
    Format.pp_print_string fmt ")";
    Format.pp_close_box fmt ()


let rec format = function
  | Atom s -> Pp.text (Sexplib.Pre_sexp.maybe_esc_str s)
  | List l when List.for_all ~f:is_atom l -> Pp.fgrp (par l)
  | List l -> Pp.agrp (par l)
and par l =
  Pp.text "(" $ Pp.nest indent (Pp.list ~sep:Pp.break ~f:format l) $ Pp.text ")"

let to_string_hum' sexp = Pp.to_string (format sexp)

module Diff :
sig
  val print : ?oc:out_channel -> Sexp.t -> Sexp.t -> unit
end
=
struct
  type diff =
      | Different of Sexp.t * Sexp.t
      | List of diff list
      | Record of record_field list

  and record_field =
      | New_in_first of Sexp.t
      | Not_in_first of Sexp.t
      | Bad_match of string * diff

  let rec rev_map_append f lst1 lst2 =
    match lst1 with
    | [] -> lst2
    | h :: t -> rev_map_append f t (f h :: lst2)

  let make_tail make tail acc =
    Some (Record (List.rev (rev_map_append make tail acc)))

  let recf (k, v) = Sexp.List [Sexp.Atom k; v]

  let maybe_record sexps =
    let is_list_of_atom_pairs = function
      | Sexp.List [Sexp.Atom _; _] -> true
      | _ -> false
    in
    sexps <> [] &&
      (List.for_all ~f:is_list_of_atom_pairs sexps)

  let sort_record_fields sexp_list =
    let to_pair = function
      | Sexp.List [Sexp.Atom k; v] -> k, v
      | _ -> assert false  (* impossible *)
    in
    let pairs = List.map ~f:to_pair sexp_list in
    List.sort ~cmp:(fun (k1, _) (k2, _) -> compare k1 k2) pairs

  let rec of_record_fields acc pairs1 pairs2 =
    match pairs1, pairs2 with
    | [], [] when acc = [] -> None
    | [], [] -> Some (Record (List.rev acc))
    | tail, [] -> make_tail (fun kv -> New_in_first (recf kv)) tail acc
    | [], tail -> make_tail (fun kv -> Not_in_first (recf kv)) tail acc
    | (((k1, v1) as h1) :: t1 as l1), (((k2, v2) as h2) :: t2 as l2) ->
        let c = compare k1 k2 in
        if c = 0 then
          match of_sexps v1 v2 with
          | None -> of_record_fields acc t1 t2
          | Some diff -> of_record_fields (Bad_match (k1, diff) :: acc) t1 t2
        else if c < 0 then of_record_fields (New_in_first (recf h1) :: acc) t1 l2
        else of_record_fields (Not_in_first (recf h2) :: acc) l1 t2

  and of_lists acc l1 l2 =
    match l1, l2 with
    | [], [] when acc = [] -> None
    | [], [] -> Some (List (List.rev acc))
    | [], _ | _, [] -> assert false  (* impossible *)
    | h1 :: t1, h2 :: t2 ->
        match of_sexps h1 h2 with
        | None -> of_lists acc t1 t2
        | Some res -> of_lists (res :: acc) t1 t2

  and of_sexps sexp1 sexp2 =
    match sexp1, sexp2 with
    | Sexp.List [], Sexp.List [] -> None
    | Sexp.Atom a1, Sexp.Atom a2 when a1 = a2 -> None
    | Sexp.List l1, Sexp.List l2 ->
        if maybe_record l1 && maybe_record l2 then
          of_record_fields [] (sort_record_fields l1) (sort_record_fields l2)
        else if List.length l1 = List.length l2 then of_lists [] l1 l2
        else Some (Different (sexp1, sexp2))
    | _ -> Some (Different (sexp1, sexp2))

  let print_t ?(oc = stdout) = function
    | None -> ()
    | Some diff ->
        let print_string ~tag ~indent str =
          Printf.fprintf oc "%-*s %s\n%!" indent tag str
        in
        let print_sexp ~tag ~indent sexp =
          print_string ~tag ~indent (Sexp.to_string sexp)
        in
        let rec loop indent = function
          | Different (sexp1, sexp2) ->
              print_sexp ~tag:"+" ~indent sexp1;
              print_sexp ~tag:"-" ~indent sexp2
          | List lst ->
              print_string ~tag:"" ~indent "(";
              List.iter ~f:(loop (indent + 1)) lst;
              print_string ~tag:"" ~indent ")"
          | Record record_fields ->
              let rec print_record_field = function
                | New_in_first sexp -> print_sexp ~tag:"+" ~indent sexp
                | Not_in_first sexp -> print_sexp ~tag:"-" ~indent sexp
                | Bad_match (key, diff) ->
                    print_string ~tag:"" ~indent key;
                    loop (indent + 1) diff
              in
              List.iter ~f:print_record_field record_fields;
        in
        loop 0 diff

  let print ?oc sexp1 sexp2 = print_t ?oc (of_sexps sexp1 sexp2)
end

let print_diff ?oc sexp1 sexp2 = Diff.print ?oc sexp1 sexp2

(* The purpose of this module is just to group this craziness together. *)
module Summarize = struct
  (* An arbitrary distance metric between the nodes of an sexp, which is thought of as a
     tree.  Take a description of the path: `Pos i means move to the ith element of a list,
     `Back i means the current node is the ith element of its parents list and move to that
     parent. *)
  let rec path_depth = function
    | `Found -> 0
    | `Pos (_, path) -> 1 + path_depth path
    | `Back (i, (`Pos (n, path))) -> 1 + (min (abs (n-i)) i) + path_depth path
    | `Back (i, path) -> 1 + min 3 i + path_depth path
  ;;

  let dot_dot_dot = Sexp.Atom "...";;

  (* returns the parts of sexp that are "close" to the part of the sexp that path points
     to. *)
  let rec neighbors sexp path max_distance =
    match sexp, max_distance with
    | Sexp.Atom _, 0 -> failwith "Bug"
    | Sexp.Atom str, depth ->
        (* large atoms are more distant *)
        let length_punishment = float (max (String.length str - 3) 0) /. 10. in
        (* let length_punishment = log (float (String.length str)) /. 1.8 in *)
        let my_distance = float (path_depth path) +. length_punishment in
        if my_distance < float depth
        then Sexp.Atom str
        else dot_dot_dot
    (* performance hack: if a list is going to contain all "..." atoms, then "..." the list
       itself *)
    | Sexp.List _, (0 | 1) -> dot_dot_dot
    | Sexp.List sexps, max_distance ->
        if path_depth path >= max_distance then dot_dot_dot
        else
          let sexps =
            List.mapi sexps
              ~f:(fun i sexp ->
                let new_path =
                  match path with
                  | `Found | `Back _ -> `Back (i, path)
                  | `Pos (n, path) ->
                      if n = i
                      then path
                      else `Back (i, `Pos (n, path))
                in
                neighbors sexp new_path max_distance
              )
          in
          let sexps =
            (* consolidate consecutive "..." atoms into one "..." atom *)
            List.fold sexps ~init:[]
              ~f:(fun accum sexp ->
                match accum with
                | [] -> [ sexp ]
                | hd :: _tl ->
                    if phys_equal sexp dot_dot_dot && phys_equal hd dot_dot_dot
                    then accum
                    else sexp :: accum
              ) |! List.rev
          in
          (* replace "(...)" with just "..." *)
          if sexps = [ dot_dot_dot ] then dot_dot_dot
          else Sexp.List sexps
  ;;

  (* given an sexp, an "erroneous" sub_sexp, and a maximum distance, returns an sexp of
     nodes near sub_sexp. *)
  let summarize_sexp sexp sub_sexp depth =
    let search_result = Sexp.search_physical sexp ~contained:sub_sexp in
    match search_result with
    | `Not_found ->
        failwithf "Sexp %s not found in sexp %s" (Sexp.to_string sub_sexp) (Sexp.to_string sexp) ()
    | (`Found | `Pos _) as path ->
        let subst = Sexp.List [ Sexp.Atom "ERROR-->"; sub_sexp; Sexp.Atom "<--ERROR" ] in
        let annotated_sexp = Sexp.subst_found sexp ~subst path in
        let rec new_path path =
          match path with
          | `Pos (n, path) -> `Pos (n, new_path path)
          | `Found -> `Pos (1, `Found)
        in
        neighbors annotated_sexp (new_path path) depth

  (* Could be replaced by a faster estimate of the size *)
  let sexp_size sexp =
    let sexp_library_is_buggy = true in
    if sexp_library_is_buggy
    then String.length (Sexp.to_string sexp)
    else snd (Sexp.size sexp)
  ;;

  let rec my_sexp_size = function
    | Sexp.List l ->
        List.fold l ~init:2
          ~f:(fun sum sexp -> sum + my_sexp_size sexp)
    | Sexp.Atom str -> String.length str  (* should really be +2 if spaces present *)
  (* should add 1 for the space between two adjacent atoms *)
  ;;

  (* summarizes sexp to have a maximum string length *)
  let summarize_sexp_length sexp sub_sexp length =
    let is_too_big max_depth =
      let sexp = summarize_sexp sexp sub_sexp max_depth in
      my_sexp_size sexp > length
    in
    let rec binary_search lower_bound upper_bound =
      if upper_bound = Some (lower_bound + 1) then lower_bound
      else
        let depth_to_try =
          match upper_bound with
          | None -> lower_bound * 2
          | Some upper_bound -> (lower_bound + upper_bound) / 2
        in
        if is_too_big depth_to_try
        then binary_search lower_bound (Some depth_to_try)
        else binary_search depth_to_try upper_bound
    in
    let perfect_depth = binary_search 1 None in
    summarize_sexp sexp sub_sexp perfect_depth
  ;;
end

let summarize sexp ~sub_sexp ~size =
  match size with
  | `string s ->
      Summarize.summarize_sexp_length sexp sub_sexp s
  | `depth d ->
      Summarize.summarize_sexp sexp sub_sexp d

let of_sexp_allow_extra_fields of_sexp sexp =
  let rcef = Sexplib.Conv.record_check_extra_fields in
  let prev = !rcef in
  protect ~finally:(fun () -> rcef := prev)
    ~f:(fun () ->
      rcef := false;
      of_sexp sexp
    )

exception Filter_record_sexp_got_non_record of Sexp.t * string list with sexp
exception Filter_record_sexp_found_no_matching_fields of Sexp.t * string list with sexp


let filter_record_sexp sexp field_names =
  let raise_non_record () =
    raise (Filter_record_sexp_got_non_record (sexp, field_names))
  in
  let raise_no_matching_fields () =
    raise (Filter_record_sexp_found_no_matching_fields (sexp, field_names))
  in
  match sexp with
  | Sexp.List fields ->
    let l =
      List.filter fields ~f:(function
        | Sexp.List (Sexp.Atom name :: _) -> List.mem ~set:field_names name
        | _ -> raise_non_record ())
    in
    if List.is_empty l then raise_no_matching_fields ()
    else Sexp.List l
  | _ -> raise_non_record ()
;;

let of_generated_sexp of_sexp ~original_sexp ~generated_sexp =
  try of_sexp generated_sexp with
  | Of_sexp_error (exn, error_sexp) ->
    let error_sexp =
      if phys_equal error_sexp generated_sexp then
        original_sexp
      else
        error_sexp
    in
    raise (Of_sexp_error (exn, error_sexp))
;;

let filter_record t_of_sexp names =
  fun sexp ->
    of_generated_sexp t_of_sexp
      ~original_sexp:sexp
      ~generated_sexp:(filter_record_sexp sexp names)
;;

module Records_table = struct
  type 'a t = 'a list with sexp

  type 'a sexpable = 'a t

  exception Invalid_record_sexp of Sexp.t with sexp

  exception Record_sexp_missing_field of (string * Sexp.t) list * string
  with sexp

  let sexp_of_t sexp_of_record records =
    let records =
      List.map records ~f:(fun record ->
        let sexp = sexp_of_record record in
        match sexp with
        | Sexp.List fields ->
          List.map fields ~f:(function
            | Sexp.List [ Sexp.Atom name; value ] -> (name, value)
            | _ -> raise (Invalid_record_sexp sexp))
        | _ -> raise (Invalid_record_sexp sexp))
    in
    let rows =
      match records with
      | [] -> []
      | record :: _ ->
        let field_names = List.map record ~f:fst in
        let header =
          Sexp.List (List.map field_names ~f:(fun field_name ->
            Sexp.Atom field_name))
        in
        let tuples =
          List.map records ~f:(fun record ->
            Sexp.List
              (List.map field_names ~f:(fun field_name ->
                match
                  List.find_map record ~f:(fun (field_name', value) ->
                    if field_name = field_name' then Some value else None)
                with
                | None -> raise (Record_sexp_missing_field (record, field_name))
                | Some value -> value)))
        in
        header :: tuples
    in
    Sexp.List rows
  ;;

  exception Invalid_table_sexp with sexp

  let t_of_sexp record_of_sexp sexp =
    let error () = raise (Of_sexp_error (Invalid_table_sexp, sexp)) in
    let rows =
      match sexp with
      | Sexp.Atom _ -> error ()
      | Sexp.List l ->
        List.map l ~f:(fun row ->
          match row with
          | Sexp.Atom _ -> error ()
          | Sexp.List l -> l)
    in
    match rows with
    | [] -> error ()
    | header :: rest ->
      List.map rest ~f:(fun tuple ->
        of_generated_sexp record_of_sexp
          ~original_sexp:sexp
          ~generated_sexp:
          (Sexp.List
             (List.map2_exn header tuple ~f:(fun field value ->
               Sexp.List [field; value]))))
  ;;
end
