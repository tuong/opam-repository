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

let foldi ~init ar ~f =
  fst (Array.fold ~init:(init,0) ar ~f:(fun (a,i) x -> f i a x,i+1))

let random_split ?random_state ~p array =
  let a = Array.copy array in
  if p > 1.0 || p < 0.0 then
    failwith "Array.random_split: p is out of bounds [0 1]";
  let stop = Float.iround_exn (p *. (float (Array.length a))) in
  if stop = 0 then
    (* in slice a stop of 0 means slicing to the end of the array, which is not what we
       want *)
    ([||], a)
  else
    begin
      Array.permute ?random_state a;
      ((Array.slice a 0 stop), (Array.slice a stop 0))
    end

let random_sub ?random_state ~p array =
  fst (random_split ?random_state array ~p)
