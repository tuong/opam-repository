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

module Array = StdLabels.Array
open Sexplib.Wrapper
open Bin_prot.Std

let invalid_argf = Core_printf.invalid_argf

type 'a t = 'a array with sexp, bin_io

type 'a binable = 'a t
type 'a container = 'a t
type 'a sexpable = 'a t

(* Standard functions *)
let append = Array.append
let blit = Array.blit
let concat = Array.concat
let copy = Array.copy
let create_matrix = Array.create_matrix
let fill = Array.fill
let fold_right = Array.fold_right
let init = Array.init
let iteri = Array.iteri
let make_matrix = Array.make_matrix
let map = Array.map
let mapi = Array.mapi
let of_list = Array.of_list
(* Note that Ocaml's stable_sort and fast_sort are the same. Regular sort is unstable and
   slower, but uses constant heap space. *)
let sort = Array.sort
let stable_sort = Array.stable_sort
let sub = Array.sub
let to_list = Array.to_list

external create : int -> 'a -> 'a array = "caml_make_vect"

let create i x =
  try create i x
  with Invalid_argument _ ->
    invalid_argf "Array.create %d: invalid length" i ()
;;

external get : 'a array -> int -> 'a = "%array_safe_get"
external set : 'a array -> int -> 'a -> unit = "%array_safe_set"
external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"
external length : 'a array -> int = "%array_length"

let max_length = Sys.max_array_length

let to_array t = t

let is_empty t = length t = 0

let fold t ~init ~f = Array.fold_left t ~init ~f

let iter t ~f = Array.iter t ~f

let concat_map t ~f = concat (to_list (map ~f t))

(** [normalize array index] returns a new index into the array such that if index is less
    than zero, the returned index will "wrap around" -- i.e. array.(normalize array (-1))
   returns the last element of the array. *)

let normalize t i =
  Ordered_collection_common.normalize ~length_fun:length t i

(** [slice array start stop] returns a fresh array including elements [array.(start)]
    through [array.(stop-1)] with the small tweak that the start and stop positions are
    normalized and a stop index of 0 means the same thing a stop index of
    [Array.length array].  In summary, it's like the slicing in Python or Matlab. *)
let slice t start stop =
  Ordered_collection_common.slice ~length_fun:length ~sub_fun:sub
    t start stop

(** [nget array index] "normalizes" the index to {!Array.get} -- see normalize *)
let nget t i =
  t.(normalize t i)

(** [nset array index value] "normalizes" the index to {!Array.set} -- see normalize *)
let nset t i v =
  t.(normalize t i) <- v

let swap = Array_permute.swap;;

(** reverses an array in place. *)

let rev_inplace t =
  let n = length t in
  if n > 1 then
    for i = 0 to (n - 1) / 2 do
      let j = n - 1 - i in
      swap t i j
    done

let of_list_rev l =
  let t = of_list l in
  rev_inplace t;
  t

(** [filter_opt array] returns a new array where [None] entries are omitted and [Some x]
    entries are replaced with [x]. Note that this changes the index at which elements
    will appear. *)
let filter_opt t =
  let result = ref [] in
  iter t ~f:(function
    | None -> ()
    | Some x -> result := x :: !result
  );
  of_list_rev !result

(* The following implementation of filter_opt is faster, and as far as we know it's
   correct, but we don't want to code review it yet until we actually need the speed.
let filter_opt t =
  let lix = length t - 1 in
  let rec outer_loop outer_i =
    if outer_i < 0 then [||]
    else
      match t.(outer_i) with
      | None -> outer_loop (outer_i - 1)
      | Some el ->
          let rec loop i n =
            if i < 0 then
              let res = make n el in
              let rec inner_loop i pos =
                if i < 0 then res
                else
                  match t.(i) with
                  | None -> inner_loop (i - 1) pos
                  | Some el ->
                      res.(pos) <- el;
                      inner_loop (i - 1) (pos - 1)
              in
              inner_loop (outer_i - 1) (n - 2)
            else
              match t.(i) with
              | None -> loop (i - 1) n
              | Some _ -> loop (i - 1) (n + 1)
          in
          loop (outer_i - 1) 1
  in
  outer_loop lix
*)

(** [filter_map ~f array] maps [f] over [array] and filters [None] out of the results. *)
let filter_map t ~f = filter_opt (map t ~f)
(** Same as {!filter_map} but uses {!Array.mapi}. *)
let filter_mapi t ~f = filter_opt (mapi t ~f)

let iter2 t1 t2 ~f =
  if length t1 <> length t2 then invalid_arg "Array.iter2";
  iteri t1 ~f:(fun i x1 -> f x1 t2.(i))

let map2 t1 t2 ~f =
  let len = length t1 in
  if length t2 <> len then invalid_arg "Array.map2";
  init len ~f:(fun i -> f t1.(i) t2.(i))

(** [filter ~f array] removes the elements for which [f] returns false.  *)
let filter ~f =  filter_map ~f:(fun x -> if f x then Some x else None)
(** Like {!filter} except [f] also receives the index. *)
let filteri ~f = filter_mapi ~f:(fun i x -> if f i x then Some x else None)

let exists t ~f =
  let rec loop i =
    if i < 0
    then false
    else if f t.(i)
    then true
    else loop (i - 1)
  in
  loop (length t - 1)

let mem el t =
  exists t ~f:(fun x -> x = el)

let for_all t ~f =
  let rec loop i =
    if i < 0
    then true
    else if f t.(i)
    then loop (i - 1)
    else false
  in
  loop (length t - 1)

let for_all2 t1 t2 ~f =
  let len = length t1 in
  if length t2 <> len then invalid_arg "Array.for_all2";
  let rec loop i =
    if i < 0
    then true
    else if f t1.(i) t2.(i)
    then loop (i - 1)
    else false
  in
  loop (len - 1)


let replace t i ~f = t.(i) <- f t.(i)

(** modifies an array in place -- [t.(i)] will be set to [f(t.(i))] *)
let replace_all t ~f =
  for i = 0 to length t - 1 do
    t.(i) <- f t.(i)
  done

let findi t ~f =
  let length = length t in
  let rec loop i =
    if i >= length then None
    else if f t.(i) then Some i
    else loop (i + 1)
  in
  loop 0
;;

let findi_exn t ~f =
  match findi t ~f with
  | None -> raise Not_found
  | Some x -> x
;;

let find_exn t ~f =
  match findi t ~f with
  | None -> raise Not_found
  | Some i -> t.(i)
;;

let find t ~f = Option.map (findi t ~f) ~f:(fun i -> t.(i))

let reduce t ~f =
  if length t = 0 then None
  else begin
    
    let r = ref t.(0) in
    for i = 1 to length t - 1 do
      r := f !r t.(i)
    done;
    Some !r
  end

let reduce_exn t ~f =
  match reduce t ~f with
  | None -> invalid_arg "Array.reduce_exn"
  | Some v -> v

let permute = Array_permute.permute

let combine t1 t2 =
  if length t1 <> length t2 then failwith "Array.combine"
  else map2 t1 t2 ~f:(fun x1 x2 -> x1, x2)

let split t =
  let n = length t in
  if n = 0 then [||], [||]
  else
    let x, y = t.(0) in
    let res1 = create n x in
    let res2 = create n y in
    for i = 1 to n - 1 do
      let x, y = t.(i) in
      res1.(i) <- x;
      res2.(i) <- y;
    done;
    res1, res2

let sorted_copy t ~cmp =
  let t1 = copy t in
  sort t1 ~cmp;
  t1

let last t = t.(length t - 1)

module Infix = struct
  let ( <|> ) t (start,stop) = slice t start stop
end

(* We use [init 0] rather than [||] because all [||] are physically equal, and
   we want [empty] to create a new array. *)
let empty () = init 0 ~f:(fun _ -> assert false)

let cartesian_product t1 t2 =
  if is_empty t1 || is_empty t2 then
    empty ()
  else
    let n1 = length t1 in
    let n2 = length t2 in
    let t = create (n1 * n2) (t1.(0), t2.(0)) in
    let r = ref 0 in
    for i1 = 0 to n1 - 1 do
      for i2 = 0 to n2 - 1 do
        t.(!r) <- (t1.(i1), t2.(i2));
        incr r;
      done
    done;
    t
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
