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

open OUnit


let unused_test = Timer_test.test

let all =
  TestList
    [
      Avltree_test.test;
      Bag_test.test;
      Bigbuffer_test.test;
      Bigstring_test.test;
      Binary_packing_test.test;
      Blang_test.test;
      Common_test.test;
      Comparable_test.test;
      Crc_test.test;
      Doubly_linked_test.test;
      Float_test.test;
      Fqueue_test.test;
      Hashtbl_test.test;
      Hash_queue_test.test;
      Heap_test.test;
      Interval_test.test;
      Int_conversions_test.test;
      Hash_heap_test.test;
      Core_int_test.test;
      Core_array_test.test;
      Core_filename_test.test;
      Core_map_test.test;
      Core_set_test.test;
      Core_list_test.test;
      Core_queue_test.test;
      Core_string_test.test;
      Core_mutex_test.test;
      
      PMap_test.test;
      PSet_test.test;
      Time_test.test;
      TZ_test.test;
      Int_set_test.test;
      Core_float_test.test;
      Core_unix_test.test;
    ]
