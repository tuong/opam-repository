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

open OUnit;;
open Core.Std
open Core_extended.Std

open Union_find

let test =
  "union_find" >:::
    [
      "all" >::
        (fun () ->
          let t1 = create 13 in
          assert (get t1 = 13);
          set t1 15;
          assert (get t1 = 15);
          assert (same_class t1 t1);
          let t2 = create 17 in
          assert (not (same_class t1 t2));
          union t1 t2;
          assert (same_class t1 t1);
          assert (same_class t1 t2);
          assert (same_class t2 t2);
          assert (get t1 = get t2);
          assert (get t1 = 15 || get t1 = 17);
          let t3 = create 19 in
          union t1 t3;
          let t4 = create 21 in
          assert (not (same_class t3 t4));
          union t1 t4;
          assert (same_class t3 t4);
        );
    ]
;;
