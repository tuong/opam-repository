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

(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
open Core.Std


type 'a t = { mutable data: 'a array; (** base of circular buffer *)
              mutable start: int;  (** first position at which data is found *)
              mutable length: int; (** number of elements in buffer *)
              never_shrink: bool; (** whether to refrain from shrinking the buffer *)
              dummy: 'a; (** value used to pack into newly allocated arrays *)
            }
with sexp

let create ?(never_shrink=false) dummy length =
  { data = Array.create (Int.max 10 length) dummy;
    start = 0;
    length = 0;
    never_shrink = never_shrink;
    dummy = dummy;
  }

let length buf = buf.length

let phys_length buf = Array.length buf.data

let is_full buf = buf.length >= Array.length buf.data

let check_index fname buf i =
  if i < 0 || i >= buf.length then
    invalid_arg (Printf.sprintf "Cbuffer.%s: index %i is not between 0 and %d"
                   fname i (buf.length - 1))

let get buf i =
  check_index "get" buf i;
  buf.data.((buf.start + i) mod phys_length buf)

let set buf i v =
  check_index "set" buf i;
  buf.data.((buf.start + i) mod phys_length buf) <- v

let copy_data ~src ~dst start length =
  if start + length <= Array.length src
  then Array.blit ~src ~dst ~src_pos:start ~dst_pos:0 ~len:length
  else
    let length1 = Array.length src - start in
    let length2 = length - length1 in
    Array.blit ~src ~dst ~src_pos:start ~dst_pos:0 ~len:length1;
    Array.blit ~src ~dst ~src_pos:0 ~dst_pos:length1 ~len:length2

(* [swap_array buf len] copies the contents of [buf] to a new array of length [len] and
   places that new data into the buffer *)
let swap_array buf new_length =
  let newdata = Array.create new_length buf.dummy in
  copy_data  ~src:buf.data ~dst:newdata buf.start buf.length;
  buf.data <- newdata;
  buf.start <- 0

(** double the size of the buffer *)
let expand buf = swap_array buf (phys_length buf * 2)

(** half the size of the buffer *)
let shrink buf =
  if buf.length > phys_length buf / 2 then
    invalid_arg (Printf.sprintf "Cbuffer.shrink: buffer is too big (%d > %d)"
                   buf.length (phys_length buf / 2))
  else
    swap_array buf (phys_length buf / 2)

let to_array buf =
  let ar = Array.create buf.length buf.dummy in
  copy_data ~src:buf.data ~dst:ar buf.start buf.length;
  ar

let add buf v =
  if is_full buf then expand buf;
  if buf.start = 0 then
    buf.start <- phys_length buf - 1
  else buf.start <- buf.start - 1;
  buf.data.(buf.start) <- v;
  buf.length <- buf.length + 1

let drop_from buf i =
  check_index "drop_from" buf i;
  for j = i to buf.length - 1 do
    set buf j buf.dummy
  done;
  buf.length <- i;
  if not buf.never_shrink && buf.length < phys_length buf / 4
  then shrink buf

let drop_last buf =
  drop_from buf (buf.length - 1)

(****** not well code-reviewed from here down ******)
let iter buf ~f =
  for i = 0 to (length buf) - 1 do
    f i (get buf i);
  done

let iterr buf ~f =
  for i = (length buf) - 1 downto 0 do
    f i (get buf i);
  done

let of_array arr =
  let len = Array.length arr in
  if len = 0 then invalid_arg "Cbuffer.of_array: empty array argument";
  let buf = create arr.(0) len in
  (* we do not have access to buf.data here -- reimplement blit! *)
  (* Array.blit ~src:arr ~dst:buf.data ~src_pos:0 ~dst_pos:0 ~len; *)
  for i =  len - 1 downto 0 do add buf arr.(i) done;
  buf

let rec cb_compare ~f ~b1 ~b2 ~s1 ~s2 ~n =
  if n = 0 then true
  else if (length b1) - s1 < n || (length b2) - s2 < n then false
  else if f (get b1 s1) (get b2 s2)
  then cb_compare ~f ~b1 ~b2 ~s1:(s1+1) ~s2:(s2+1) ~n:(n-1)
  else false

let drop_old ?(cmp = compare) ?free ~f ~cutoff buf =
  (* should be ?(f=ident), but then ocaml thinks that f is 'a -> 'a *)
  let len = length buf in
  let last = ref len in
  while !last > 0 && cmp (f (get buf (!last - 1))) cutoff <= 0 do
    decr last;
    Option.iter free ~f:(fun f -> f (get buf !last));
  done;
  if !last < len then drop_from buf !last;
  len - !last                   (* number of objects dropped *)
    
