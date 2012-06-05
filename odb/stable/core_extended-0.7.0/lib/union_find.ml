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

(* This code is based on the MLton library set/disjoint.fun, which has the
   following copyright notice.
*)
(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)
open Core.Std

type 'a root = {
  mutable value: 'a;
  mutable rank: int;
}

let root_equal (r : _ root) r' = phys_equal r r'

type 'a t = {
  mutable parent : 'a parent;
}
and 'a parent =
| Parent of 'a t
| Root of 'a root

let create v = { parent = Root { value = v; rank = 0; }; }

let compress t =
  let rec loop t ac =
    match t.parent with
    | Root _ ->
        let p = Parent t in
        List.iter ac ~f:(fun t -> t.parent <- p);
    | Parent t' -> loop t' (t :: ac)
  in
  loop t []
;;

let root t =
  compress t;
  match t.parent with
  | Root r -> r
  | Parent t ->
      match t.parent with
      | Root r -> r
      | Parent _ -> assert false
;;
      
let get t = (root t).value

let set t v = (root t).value <- v

let same_class t1 t2 = root_equal (root t1) (root t2)

let union t1 t2 =
  let r1 = root t1 in
  let r2 = root t2 in
  if root_equal r1 r2 then
    ()
  else
    let n1 = r1.rank in
    let n2 = r2.rank in
    if n1 < n2 then
      t1.parent <- Parent t2
    else begin
      t2.parent <- Parent t1;
      if n1 = n2 then r1.rank <- r1.rank + 1;
    end
;;
