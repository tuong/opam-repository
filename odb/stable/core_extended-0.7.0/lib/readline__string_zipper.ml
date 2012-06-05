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

type t = char Readline__list_zipper.t

open Readline__list_zipper

let drop_before = drop_before
let drop_after = drop_after
let insert_before = insert_before
let insert_after = insert_after
let previous = previous
let next = next

let contents zip =
  let ll = List.length zip.l
  and lr = List.length zip.r in
  let res = String.create (ll+lr) in
  List.iteri zip.l
    ~f:(fun i c -> res.[ll-1-i] <- c);
  List.iteri zip.r
    ~f:(fun i c -> res.[ll+i] <- c);
  res

let left_contents zip =
  let len = List.length zip.l in
  let res = String.create len in
  List.iteri zip.l
    ~f:(fun i c -> res.[len-1-i] <- c);
  res

let right_contents zip =
  let len = List.length zip.r in
  let res = String.create len in
  List.iteri zip.r
    ~f:(fun i c -> res.[i] <- c);
  res

let first zip =
  {
    l = [];
    r = List.rev zip.l @ zip.r;
  }

let last zip =
  {
    l = List.rev zip.r @ zip.l;
    r = [];
  }

let create left right =
  {
    l = String.to_list_rev left;
    r = String.to_list right
  }
