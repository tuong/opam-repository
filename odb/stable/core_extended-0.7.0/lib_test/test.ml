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

let all = TestList [
  Cbuffer_test.test;
  Condition_test.test;
  Extended_array_test.test;
  Extended_filename_test.test;
  Extended_float_test.test;
  Extended_list_test.test;
  Extended_string_test.test;
  Extended_time_test.test;
  Escaping_test.test;
  Iter_test.test;
  Ldd_test.test;
  Cache_test.test;
  Shell_test.test;
  Search_test.test;
  Union_find_test.test;
  Numbers_test.test;
  Bin_io_utils_test.test;
]
