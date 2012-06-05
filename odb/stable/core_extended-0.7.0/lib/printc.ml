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

let (~:) l = String.concat l
let (~%) = sprintf

let print = print_string
let fprint = output_string
let eprint x = output_string stderr x

let printl s = print s; print "\n"; flush stdout
let fprintl f s = fprint f s; fprint f "\n"; flush stdout
let eprintl s = eprint s; eprint "\n"; flush stderr

let pad side ~fill n s =
  let fill = match fill with
    | None -> ' ' | Some c -> c
  in
  let orig_len = String.length s in
  if orig_len >= n then s
  else
    let s' = String.make n fill in
    let dst_pos = match side with
      | `left -> n - orig_len
      | `right -> 0
    in
    String.blit ~src:s ~dst:s' ~src_pos:0 ~dst_pos ~len:orig_len;
    s'

let lpad ?fill n s = pad `left ?fill n s
let rpad ?fill n s = pad `right ?fill n s

let i2s = Int.to_string
let f2s = Float.to_string

(*
let f2s fmt f =
  let fmt = Scanf.sscanf (~:["\"";fmt;"\""]) "%{%f%}" (fun i -> i) in
  sprintf fmt f
*)
