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


(* execute function supressing compactions. the doc says:
   mutable max_overhead : int;
   Heap compaction is triggered when the estimated amount of "wasted" memory
   is more than max_overhead percent of the amount of live data.
   If max_overhead is set to 0, heap compaction is triggered at the end of each
   major GC cycle (this setting is intended for testing purposes only).
   If max_overhead >= 1000000, compaction is never triggered.
   Default: 500. *)
let without_compactions ?logger ~f a =
  (* note that f may call get/set itself, and the fields in the Gc.control
     struct are mutable, so it does not make much sense to save it *)
  protectx (Gc.get ()).Gc.Control.max_overhead
    ~f:(fun _ ->
      Gc.tune ?logger ~max_overhead:1000000 ();
      f a)
    ~finally:(fun max_overhead -> Gc.tune ?logger ~max_overhead ())

