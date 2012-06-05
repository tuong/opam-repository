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

include Fold_map.Make2_sexpable
(struct
   type 'a data = 'a
   type 'a t = 'a list with sexp
   type 'a sexpable = 'a t
   let init = []
   let f list x = x :: list
 end)



let iter ~f m =
  iter ~f:(fun ~key ~data -> List.iter data ~f:(fun data -> f ~key ~data)) m

let mapi ~f m =
  of_map (Map.mapi (to_map m)
             ~f:(fun ~key ~data -> List.map ~f:(fun data -> f ~key ~data) data))

let map ~f m =
  of_map (Map.map (to_map m) ~f:(List.map ~f))

let fold ~f m ~init =
  fold m
    ~f:(fun ~key ~data acc ->
      List.fold data
        ~f:(fun acc data -> f ~key ~data acc)
        ~init:acc)
    ~init

let set ~key ~data m =
  if data=[] then
    remove m key
  else
    set ~key ~data m

let filter ~f m =
  of_map
    (Map.filter_mapi (to_map m)
        ~f:(fun ~key ~data ->
          let data = List.filter data
            ~f:(fun data -> f ~key ~data)
          in
          if data = [] then
            None
          else
            Some data))

let reduce ~f m = Map.map ~f (to_map m)
