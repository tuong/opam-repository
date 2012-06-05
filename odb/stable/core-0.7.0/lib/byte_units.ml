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

(* Conversions between units of measure based on bytes. *)

open Sexplib.Wrapper
open Bin_prot.Std

module T = struct
  type t = [
      | `Bytes of float
      | `Kilobytes of float
      | `Megabytes of float
      | `Gigabytes of float
      | `Words of float
  ] with bin_io, sexp

  type sexpable = t
  type binable = t

  let bytes_per_word =
    let module W = Word_size in
    match W.word_size with
    | W.W32 -> 4.
    | W.W64 -> 8.
  ;;

  let kbyte = 1024.
  let mbyte = kbyte *. kbyte
  let gbyte = kbyte *. mbyte

  let bytes = function
    | `Bytes n -> n
    | `Kilobytes n -> n *. kbyte
    | `Megabytes n -> n *. mbyte
    | `Gigabytes n -> n *. gbyte
    | `Words n -> n *. bytes_per_word

  let kilobytes t = bytes t /. kbyte
  let megabytes t = bytes t /. mbyte
  let gigabytes t = bytes t /. gbyte
  let words t = bytes t /. bytes_per_word

  let compare t1 t2 = Float.compare (bytes t1) (bytes t2)
  

  let equal t1 t2 = bytes t1 = bytes t2
  let hash = Hashtbl.hash
end

include T
include Comparable.Make(T)
include Hashable.Make(T)
