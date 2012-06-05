(******************************************************************************
 *                             Bin-prot                                       *
 *                                                                            *
 * Copyright (C) 2008- Jane Street Holding, LLC                               *
 *    Contact: opensource@janestreet.com                                      *
 *    WWW: http://www.janestreet.com/ocaml                                    *
 *    Author: Markus Mottl                                                    *
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

open Printf
open Bigarray
open Bin_prot
open Common
open Utils
open Bin_prot.Std


(* Checks correct behavior for empty types *)
type empty with bin_io

module type S = sig
  (* Checks correct behavior for type signatures with variance annotations. *)
  type +'a t with bin_io
end

module PolyInhTest = struct
  type x = [ `X1 | `X2 ] with bin_io
  type y = [ `Y1 | `Y2 ] with bin_io
  type xy = [ x | `FOO | `Bar of int * float | y ] with bin_io
end

type tuple = float * string * int64
with bin_io

type 'a record = { a : int; b : 'a; c : 'b. 'b option }
with bin_io

type 'a singleton_record = { y : 'a }
with bin_io

type 'a sum = Foo | Bar of int | Bla of 'a * string
with bin_io

type 'a variant = [ `Foo | `Bar of int | `Bla of 'a * string ]
with bin_io

type 'a poly_app = (tuple * int singleton_record * 'a record) variant sum list
with bin_io

type 'a rec_t1 = RecFoo1 of 'a rec_t2
and 'a rec_t2 = RecFoo2 of 'a poly_app * 'a rec_t1 | RecNone
with bin_io

type 'a poly_id = 'a rec_t1
with bin_io

type el = float poly_id
with bin_io

type els = el array
with bin_io

let mb = 1024. *. 1024.

let main () =
  (* Allocate buffer (= bigstring) *)
  let buf = create_buf 10000 in

  (* Define array of dummy elements to be marshalled *)
  let el =
    let record = { a = 17; b = 2.78; c = None } in
    let arg = (3.1, "foo", 42L), { y = 4321 }, record in
    let variant = `Bla (arg, "fdsa") in
    let sum = Bla (variant, "asdf") in
    let poly_app = [ sum ] in
    RecFoo1 (RecFoo2 (poly_app, RecFoo1 RecNone))
  in
  let x = Array.create 10 el in

  let n = 100_000 in

  (* Write n times *)
  let t1 = Unix.gettimeofday () in
  for i = 1 to n do
    ignore (bin_write_els buf ~pos:0 x)
  done;
  let t2 = Unix.gettimeofday () in
  let write_time = t2 -. t1 in

  (* Read n times *)
  let t1 = Unix.gettimeofday () in
  for i = 1 to n do
    let pos_ref = ref 0 in
    ignore (bin_read_els buf ~pos_ref)
  done;
  let t2 = Unix.gettimeofday () in
  let read_time = t2 -. t1 in

  (* Write, read, and verify *)
  let end_pos = bin_write_els buf ~pos:0 x in
  let pos_ref = ref 0 in
  let y = bin_read_els buf ~pos_ref in
  assert (!pos_ref = end_pos && x = y);

  (* Print result *)
  let f_n = float n in
  let msg_size = float (n * end_pos) in
  printf
    "msgs: %d  msg length: %d\n\
    write time: %.3fs  write rate: %9.2f msgs/s  write throughput: %.2f MB/s\n\
    \ read time: %.3fs   read rate: %9.2f msgs/s   read throughput: %.2f MB/s\n%!"
    n end_pos
    write_time (f_n /. write_time) (msg_size /. write_time /. mb)
    read_time (f_n /. read_time) (msg_size /. read_time /. mb)

let () =
  try main ()
  with Read_error (err, pos) ->
    eprintf "Uncaught exception: %s: %d\n%!" (ReadError.to_string err) pos
