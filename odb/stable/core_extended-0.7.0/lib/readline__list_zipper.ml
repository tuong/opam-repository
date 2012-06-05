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

type 'a t = {
  l : 'a list;
  r : 'a list
}

let create l r = {
  l = List.rev l;
  r = r
}

let drop_before = function
  | {l = []; r= _} ->  None
  | {l = h::t ; r = r } -> Some (h,{l=t;r=r})

let drop_after = function
  | { l = _; r = [] } -> None
  | { l = l ; r = h::t } -> Some (h,{ l=l; r=t })

let insert_before z v = {z with l = v::z.l}

let insert_after z v = {z with r = v::z.r}

let previous zip =
  match drop_before zip with
  | None -> None
  | Some (e,line) -> Some (insert_after line e)

let next zip =
  match drop_after zip with
  | None -> None
  | Some (e,line) -> Some (insert_before line e)

