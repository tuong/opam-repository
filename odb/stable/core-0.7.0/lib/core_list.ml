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

module List = StdLabels.List
module String = StdLabels.String
open Sexplib.Wrapper
open Bin_prot.Std

let invalid_argf = Core_printf.invalid_argf

type 'a t = 'a list with sexp, bin_io

type 'a binable = 'a t
type 'a container = 'a t
type 'a sexpable = 'a t

(* Standard functions *)
let length = List.length
let hd_exn = List.hd
let tl_exn = List.tl

let hd t =
  match t with
  | [] -> None
  | x :: _ -> Some x
;;

let tl t =
  match t with
  | [] -> None
  | _ :: t' -> Some t'
;;

let nth t n =
  if n < 0 then None else
  let rec nth_aux t n =
    match t with
    | [] -> None
    | a::t -> if n = 0 then Some a else nth_aux t (n-1)
  in nth_aux t n
;;

let nth_exn t n =
  match nth t n with
  | None ->
      invalid_argf "List.nth_exn %d called on list of length %d"
        n (length t) ()
  | Some a -> a
;;

let rev_append = List.rev_append

let rev = function
  | [] | [_] as res -> res
  | x :: y :: rest -> rev_append rest [y; x]

let iter = List.iter
let rev_map = List.rev_map

exception Length_mismatch of string * int * int with sexp

let check_length2 name l1 l2 =
  let n1 = length l1 in
  let n2 = length l2 in
  if n1 <> n2 then
    raise (invalid_argf "length mismatch in %s: %d <> %d " name n1 n2 ())
;;

let check_length3 name l1 l2 l3 =
  let n1 = length l1 in
  let n2 = length l2 in
  let n3 = length l3 in
  if n1 <> n2 || n2 <> n3 then
    raise (invalid_argf "length mismatch in %s: %d <> %d || %d <> %d"
             name n1 n2 n2 n3 ())
;;

let iter2_exn l1 l2 ~f =
  check_length2 "iter2_exn" l1 l2;
  List.iter2 l1 l2 ~f;
;;

let rev_map2_exn l1 l2 ~f  =
  check_length2 "rev_map2_exn" l1 l2;
  List.rev_map2 l1 l2 ~f;
;;

let fold2_exn l1 l2 ~init ~f =
  check_length2 "fold2_exn" l1 l2;
  List.fold_left2 l1 l2 ~init ~f;
;;

let for_all2_exn l1 l2 ~f =
  check_length2 "for_all2_exn" l1 l2;
  List.for_all2 l1 l2 ~f;
;;

let exists2_exn l1 l2 ~f =
  check_length2 "exists2_exn" l1 l2;
  List.exists2 l1 l2 ~f;
;;

let mem = List.mem
let memq = List.memq
(* This is a copy of the code from the standard library, with an extra eta-expansion to
   avoid creating partial closures (showed up for List.filter in profiling). *)
let rev_find_all ~f l =
  let rec find ~f accu = function
    | [] -> accu
    | x :: l -> if f x then find ~f (x :: accu) l else find ~f accu l
  in
  find ~f [] l

let find_all ~f l = rev (rev_find_all ~f l)
let rev_filter = rev_find_all
let filter = find_all

let sort = List.sort
let stable_sort = List.stable_sort

let find_map t ~f =
  let rec loop = function
    | [] -> None
    | x :: l ->
        match f x with
        | None -> loop l
        | Some _ as r -> r
  in
  loop t
;;

let find t ~f =
  let rec loop = function
    | [] -> None
    | x :: l -> if f x then Some x else loop l
  in
  loop t
;;

let find_exn t ~f = List.find t ~f

let findi t ~f =
  let rec loop i t =
    match t with
    | [] -> None
    | x :: l -> if f i x then Some (i, x) else loop (i + 1) l
  in
  loop 0 t
;;

(** changing the order of arguments on some standard [List] functions. *)
let exists t ~f = List.exists t ~f
let for_all t ~f = List.for_all t ~f
let iter t ~f = List.iter t ~f

(** For the container interface. *)
let fold t ~init ~f = List.fold_left t ~f ~init
let fold_left = fold
let to_array = Caml.Array.of_list
let to_list t = t

(** Tail recursive versions of standard [List] module *)

let slow_append l1 l2 = List.rev_append (List.rev l1) l2

(* There are a few optimized list operations, here, including append and map.  There are
   basically two optimizations in play: loop unrolling, and dynamic switching between
   stack and heap allocation.

   The loop-unrolling is straight-forward, we just unroll 5 levels of the loop.  This
   makes each iteration faster, and also reduces the number of stack frames consumed per
   list element.

   The dynamic switching is done by counting the number of stack frames, and then
   switching to the "slow" implementation when we exceed a given limit.  This means that
   short lists use the fast stack-allocation method, and long-lists use a slower one that
   doesn't require stack space.
*)

let rec count_append l1 l2 count =
  match l1 with
  | []               ->                         l2
  | [x1]             -> x1                   :: l2
  | [x1; x2]         -> x1 :: x2             :: l2
  | [x1; x2; x3]     -> x1 :: x2 :: x3       :: l2
  | [x1; x2; x3; x4] -> x1 :: x2 :: x3 :: x4 :: l2
  | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
    x1 :: x2 :: x3 :: x4 :: x5 ::
      (if count > 1000
       then slow_append tl l2
       else count_append tl l2 (count + 1))

let append l1 l2 = count_append l1 l2 0

(* Rebind [@] so that uses below get our tail-recursive version rather than
   Pervasive's nontail version. *)
let (@) = append

let map_slow l ~f = List.rev (List.rev_map ~f l)

let rec count_map ~f l ctr =
  match l with
  | [] -> []
  | [x1] ->
    let f1 = f x1 in
    [f1]
  | [x1; x2] ->
    let f1 = f x1 in
    let f2 = f x2 in
    [f1; f2]
  | [x1; x2; x3] ->
    let f1 = f x1 in
    let f2 = f x2 in
    let f3 = f x3 in
    [f1; f2; f3]
  | [x1; x2; x3; x4] ->
    let f1 = f x1 in
    let f2 = f x2 in
    let f3 = f x3 in
    let f4 = f x4 in
    [f1; f2; f3; f4]
  | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
    let f1 = f x1 in
    let f2 = f x2 in
    let f3 = f x3 in
    let f4 = f x4 in
    let f5 = f x5 in
    f1 :: f2 :: f3 :: f4 :: f5 ::
      (if ctr > 1000
        then map_slow ~f tl
        else count_map ~f tl (ctr + 1))

let map l ~f = count_map ~f l 0

let (>>|) l f = map l ~f

let map2_exn l1 l2 ~f = List.rev (rev_map2_exn l1 l2 ~f)

let rev_map3_exn l1 l2 l3 ~f =
  check_length3 "rev_map3" l1 l2 l3;
  let rec loop l1 l2 l3 ac =
    match (l1, l2, l3) with
    | ([], [], []) -> ac
    | (x1 :: l1, x2 :: l2, x3 :: l3) -> loop l1 l2 l3 (f x1 x2 x3 :: ac)
    | _ -> assert false
  in
  loop l1 l2 l3 []
;;

let map3_exn l1 l2 l3 ~f = List.rev (rev_map3_exn l1 l2 l3 ~f)

let rec rev_map_append l1 l2 ~f =
  match l1 with
  | [] -> l2
  | h :: t -> rev_map_append ~f t (f h :: l2)

let fold_right l ~f ~init =
  fold ~f:(fun a b -> f b a) ~init (List.rev l)

let fold_right2_exn l1 l2 ~f ~init =
  fold2_exn (List.rev l1) (List.rev l2) ~init ~f:(fun a b c -> f b c a)
;;

let split list =
  let rec loop list l1 l2 = match list with
      [] -> (List.rev l1,List.rev l2)
    | (x,y)::tl -> loop tl (x::l1) (y::l2)
  in
  loop list [] []

let combine_exn l1 l2 = map2_exn ~f:(fun a b -> (a, b)) l1 l2

(** Additional list operations *)

let rev_mapi l ~f =
  let rec loop cnt acc = function
    | [] -> acc
    | h::t -> loop (cnt + 1) (f cnt h::acc) t
  in
  loop 0 [] l

let mapi l ~f = List.rev (rev_mapi l ~f)

let iteri l ~f =
  ignore (fold l ~init:0 ~f:(fun i x -> f i x; i + 1));
;;


let foldi l ~f ~init =
  let (_,final_accum) =
    fold
      ~f:(fun (i,real_acc) v -> (i+1, f i real_acc v))
      ~init:(0,init) l
  in final_accum

let filteri l ~f =
  List.rev (foldi l
               ~f:(fun pos acc x ->
                 if f pos x then x :: acc else acc)
               ~init:[])

let reduce l ~f = match l with
  | [] -> None
  | hd::tl -> Some (fold ~init:hd ~f tl)

let reduce_exn l ~f =
  match reduce l ~f with
  | None -> raise (Invalid_argument "List.reduce_exn")
  | Some v -> v

let groupi l ~break =
  let groups =
    foldi l ~init:[] ~f:(fun i acc x ->
      match acc with
      | [] -> [[x]]
      | current_group :: tl ->
        if break i (hd_exn current_group) x then
          [x] :: current_group :: tl  (* start new group *)
        else
          (x :: current_group) :: tl) (* extend current group *)
  in
  match groups with
  | [] -> []
  | l -> rev_map l ~f:rev

let group l ~break = groupi l ~break:(fun _ x y -> break x y)

module Test : sig end = (struct
  module I = Utest.Debug_compare (struct
    type t = int list list with sexp
    let compare = compare
  end)
  module C = Utest.Debug_compare (struct
    type t = char list list with sexp
    let compare = compare
  end)

  let () = Local_utest.register_thunk ~get_location:(fun () -> assert false) (fun () ->
    assert (I.equal (group [1;2;3;4] ~break:(fun _ x -> x = 3)) [[1;2];[3;4]]);
    assert (I.equal (group [] ~break:(fun _ -> assert false)) []);
    let mis = ['M';'i';'s';'s';'i';'s';'s';'i';'p';'p';'i'] in
    let equal_letters = [['M'];['i'];['s';'s'];['i'];['s';'s'];['i'];['p';'p'];['i']] in
    let single_letters = [['M';'i';'s';'s';'i';'s';'s';'i';'p';'p';'i']] in
    let every_three = [['M'; 'i'; 's']; ['s'; 'i'; 's']; ['s'; 'i'; 'p']; ['p'; 'i']] in
    assert (C.equal (group ~break:(<>) mis) equal_letters);
    assert (C.equal (group ~break:(fun _ _ -> false) mis) single_letters);
    assert (C.equal (groupi ~break:(fun i _ _ -> i mod 3 = 0) mis) every_three);
  )
end)

let concat_map l ~f =
  let rec aux acc = function
    | [] -> List.rev acc
    | hd::tl -> aux (rev_append (f hd) acc) tl
  in
  aux [] l

let merge l1 l2 ~cmp =
  let rec loop acc l1 l2 =
    match l1,l2 with
    | [], l2 -> rev_append acc l2
    | l1, [] -> rev_append acc l1
    | h1::t1, h2::t2 ->
        if cmp h1 h2 <= 0
        then loop (h1::acc) t1 l2
        else loop (h2::acc) l1 t2
  in
  loop [] l1 l2
;;


include struct
  (* We are explicit about what we import from the general Monad functor so that
   * we don't accidentally rebind more efficient list-specific functions.
   *)
  module Monad = Monad.Make (struct
    type 'a t = 'a list
    let bind x f = concat_map x ~f
    let return x = [x]
  end)
  open Monad
  module Monad_infix = Monad_infix
  type 'a monad = 'a t
  let ignore = ignore
  let join = join
  let bind = bind
  let (>>=) = bind
  let return = return
end

(** returns final element of list *)
let rec last_exn list = match list with
  | [x] -> x
  | _::tl -> last_exn tl
  | [] -> raise (Invalid_argument "Core_list.last")

(** optionally returns final element of list *)
let rec last list = match list with
  | [x] -> Some x
  | _::tl -> last tl
  | [] -> None

(** returns sorted version of list with duplicates removed *)

let dedup ?(compare=Pervasives.compare) list =
  let sorted = List.sort ~cmp:(fun x y -> compare y x) list in
  let rec loop list accum = match list with
    | [] -> accum
    | hd::[] -> hd::accum
    | hd1::hd2::tl ->
        if compare hd1 hd2 = 0
        then loop (hd2::tl) accum
        else loop (hd2::tl) (hd1::accum)
  in
  loop sorted []


let stable_dedup lst =
  let rec dedup_order lst leftovers already_seen =
    match lst with
    | [] -> List.rev leftovers
    | hd :: tl ->
        if Core_set.mem already_seen hd
        then dedup_order tl leftovers already_seen
        else dedup_order tl (hd :: leftovers) (Core_set.add already_seen hd)
  in
  dedup_order lst [] Core_set.empty
;;


let contains_dup ?compare lst = length (dedup ?compare lst) <> length lst

let find_a_dup ?(compare=Pervasives.compare) l =
  let sorted = List.sort ~cmp:compare l in
  let rec loop l = match l with
      [] | [_] -> None
    | hd1::hd2::tl ->
      if hd1 = hd2 then Some (hd1) else loop (hd2::tl)
  in
  loop sorted

type sexp_thunk = unit -> Sexplib.Sexp.t
let sexp_of_sexp_thunk x = x ()
exception Duplicate_found of sexp_thunk * string with sexp

let exn_if_dup ?compare ?(context="exn_if_dup") t ~to_sexp =
  Option.iter (find_a_dup ?compare t) ~f:(fun dup ->
    raise (Duplicate_found ((fun () -> to_sexp dup),context))
  )

(** Returns number of elements in list for which test function returns true *)
let count list ~f =
  fold ~init:0 ~f:(fun count el -> if f el then count + 1 else count)
    list


(** [range low high] returns integers in range from [low](inclusive) to
    [high](exclusive).  [stride] is used to specify the step size *)

let range ?(stride=1) low high =
  if stride <= 0 then
    invalid_arg "Core_list.range: stride must be positive";
  let rec loop low high accum =
    if low >= high then accum
    else loop (low + stride) high (low::accum)
  in
  List.rev (loop low high [])

(** Like [range], but for floating point *)


let frange ?(stride=1.) low high =
  if Float_robust_compare.(<=.) stride 0. then
    invalid_arg "Core_list.frange: stride must be positive";
  
  let epsilon = stride /. 1000. in
  let rec loop low high accum =
    if low > high -. epsilon then accum
    else loop (low +. stride) high (low::accum)
  in
  List.rev (loop low high [])

let init n ~f =
  if n < 0 then invalid_argf "List.init %d" n ();
  let rec loop i accum =
    assert (i >= 0);
    if i = 0 then accum
    else loop (i-1) (f (i-1)::accum)
  in
  loop n []
;;

let rev_filter_map l ~f =
  let rec loop l accum = match l with
    | [] -> accum
    | hd :: tl ->
      match f hd with
      | Some x -> loop tl (x :: accum)
      | None -> loop tl accum
  in
  loop l []
;;

let filter_map l ~f = List.rev (rev_filter_map l ~f)

let filter_opt l = filter_map l ~f:(fun x -> x)

let partition_map t ~f =
  let rec loop t fst snd =
    match t with
    | [] -> (rev fst, rev snd)
    | x :: t ->
        match f x with
        | `Fst y -> loop t (y :: fst) snd
        | `Snd y -> loop t fst (y :: snd)
  in
  loop t [] []
;;

let partition t ~f =
  let f x = if f x then `Fst x else `Snd x in
  partition_map t ~f
;;


module Assoc = struct

  type ('a, 'b) t = ('a * 'b) list with sexp

  let equal x y = compare x y = 0

  let find t ?(equal=equal) key =
    Option.map (find t ~f:(fun (key', _) -> equal key key')) ~f:snd

  let find_exn t ?(equal=equal) key =
    match find t key ~equal with
    | None -> raise Not_found
    | Some value -> value

  let mem t ?(equal=equal) key = Option.is_some (find t ~equal key)

  let remove t ?(equal=equal) key =
    filter t ~f:(fun (key', _) -> not (equal key key'))

  let add t ?(equal=equal) key value =
    (* the remove doesn't change the map semantics, but keeps the list small *)
    (key, value) :: remove t ~equal key

  let inverse t = map t ~f:(fun (x, y) -> (y, x))

  let map t ~f = List.map t ~f:(fun (key, value) -> (key, f value))

end

let sub l ~pos ~len =

  if pos < 0 || len < 0 || pos + len > length l then invalid_arg "List.sub";
  List.rev
    (foldi l ~init:[]
       ~f:(fun i acc el ->
             if i >= pos && i < (pos + len)
             then el :: acc
             else acc
          )
    )

let normalize a i =
  Ordered_collection_common.normalize ~length_fun:length a i
let slice a start stop =
  Ordered_collection_common.slice ~length_fun:length ~sub_fun:sub
    a start stop

let split_n t n =
  if n <= 0 then ([], t)
  else if n >= length t then
    (t, [])
  else
    let rec loop n t accum =
      if n = 0 then
        (accum, t)
      else
        match t with
        | [] -> assert false
        | hd :: tl -> loop (n-1) tl (hd :: accum)
    in
    let (rev_first, second) = loop n t [] in
    (List.rev rev_first, second)

let take t n = fst (split_n t n)
let drop t n = snd (split_n t n)

let rec drop_while t ~f =
  match t with
  | h :: t when f h -> drop_while t ~f
  | _ -> t

let take_while t ~f =
  let rec loop acc = function
    | hd :: tl when f hd -> loop (hd :: acc) tl
    | _ -> rev acc
  in
  loop [] t
;;

let cartesian_product list1 list2 =
  if list2 = [] then [] else
    let rec loop l1 l2 accum = match l1 with
      | [] -> accum
      | (hd::tl) ->
          loop tl l2
            (List.rev_append
               (map ~f:(fun x -> (hd,x)) l2)
               accum)
    in
    List.rev (loop list1 list2 [])

let flatten l = fold_right l ~init:[] ~f:append
let flatten_no_order l = fold l ~init:[] ~f:rev_append

let concat = flatten

let cons x l = x :: l

let is_empty l = match l with [] -> true | _ -> false

let is_sorted l ~compare =
  let rec loop l =
    match l with
    | [] | [_] -> true
    | x1 :: ((x2 :: _) as rest) ->
        compare x1 x2 <= 0 && loop rest
  in loop l
;;

module Infix = struct
  let ( @ ) = append
end


let permute ?random_state list =
  match list with
  (* special cases to speed things up in trivial cases *)
  | [] | [_] -> list
  | [ x; y ] ->
      let random_bool =
        match random_state with
        | None -> Random.bool ()
        | Some state -> Random.State.bool state
      in
      if random_bool then [ y; x ] else list
  | _ ->
      let arr = Array.of_list list in
      Array_permute.permute ?random_state arr;
      Array.to_list arr

let to_string f x =
  Sexplib.Sexp.to_string
    (sexp_of_t (fun x -> Sexplib.Sexp.Atom x) (List.map ~f x))

let compare a b ~cmp =
  let rec loop a b =
    match a, b with
    | [], [] -> 0
    | [], _  -> -1
    | _ , [] -> 1
    | x::xs, y::ys ->
      let n = cmp x y in
      if n = 0 then loop xs ys
      else n
  in
  loop a b
;;

let container = {
  Container.
  length = length;
  is_empty = is_empty;
  iter = iter;
  fold = fold;
  exists = exists;
  for_all = for_all;
  find = find;
  to_list = to_list;
  to_array = to_array;
}
