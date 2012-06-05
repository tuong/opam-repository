(******************************************************************************
 *                             Bin-prot                                       *
 *                                                                            *
 * Copyright (C) 2008- Jane Street Holding, LLC                               *
 *    Contact: opensource@janestreet.com                                      *
 *    WWW: http://www.janestreet.com/ocaml                                    *
 *    Author: Markus Mottl                                                    *
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

include Size

let bin_write_unit_ = Unsafe_write_c.bin_write_unit
let bin_read_unit_ = Unsafe_read_c.bin_read_unit
let bin_read_unit__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "unit"

let bin_write_bool_ = Unsafe_write_c.bin_write_bool
let bin_read_bool_ = Unsafe_read_c.bin_read_bool
let bin_read_bool__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "bool"

let bin_write_string_ = Unsafe_write_c.bin_write_string
let bin_read_string_ = Unsafe_read_c.bin_read_string
let bin_read_string__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "string"

let bin_write_char_ = Unsafe_write_c.bin_write_char
let bin_read_char_ = Unsafe_read_c.bin_read_char
let bin_read_char__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "char"

let bin_write_int_ = Unsafe_write_c.bin_write_int
let bin_read_int_ = Unsafe_read_c.bin_read_int
let bin_read_int__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "int"

let bin_write_float_ = Unsafe_write_c.bin_write_float
let bin_read_float_ = Unsafe_read_c.bin_read_float
let bin_read_float__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "float"

let bin_write_int32_ = Unsafe_write_c.bin_write_int32
let bin_read_int32_ = Unsafe_read_c.bin_read_int32
let bin_read_int32__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "int32"

let bin_write_int64_ = Unsafe_write_c.bin_write_int64
let bin_read_int64_ = Unsafe_read_c.bin_read_int64
let bin_read_int64__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "int64"

let bin_write_nativeint_ = Unsafe_write_c.bin_write_nativeint
let bin_read_nativeint_ = Unsafe_read_c.bin_read_nativeint
let bin_read_nativeint__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "nativeint"

let bin_write_ref_ = Unsafe_write_c.bin_write_ref
let bin_read_ref_ = Unsafe_read_c.bin_read_ref
let bin_read_ref__ _f _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "ref"

let bin_write_lazy_ = Unsafe_write_c.bin_write_lazy
let bin_read_lazy_ = Unsafe_read_c.bin_read_lazy
let bin_read_lazy__ _f _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "lazy"

let bin_write_option_ = Unsafe_write_c.bin_write_option
let bin_read_option_ = Unsafe_read_c.bin_read_option
let bin_read_option__ _f _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "option"

let bin_write_list_ = Unsafe_write_c.bin_write_list
let bin_read_list_ = Unsafe_read_c.bin_read_list
let bin_read_list__ _f _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "list"

let bin_write_array_ = Unsafe_write_c.bin_write_array
let bin_read_array_ = Unsafe_read_c.bin_read_array
let bin_read_array__ _f _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "array"

let bin_write_hashtbl_ = Unsafe_write_c.bin_write_hashtbl
let bin_read_hashtbl_ = Unsafe_read_c.bin_read_hashtbl
let bin_read_hashtbl__ _f _g _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "hashtbl"

let bin_write_bigstring_ = Unsafe_write_c.bin_write_bigstring
let bin_read_bigstring_ = Unsafe_read_c.bin_read_bigstring
let bin_read_bigstring__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "bigstring"

let bin_write_mat_ = Unsafe_write_c.bin_write_mat
let bin_read_mat_ = Unsafe_read_c.bin_read_mat
let bin_read_mat__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "mat"

let bin_write_float32_mat_ = Unsafe_write_c.bin_write_float32_mat
let bin_read_float32_mat_ = Unsafe_read_c.bin_read_float32_mat
let bin_read_float32_mat__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "float32_mat"

let bin_write_float64_mat_ = Unsafe_write_c.bin_write_float64_mat
let bin_read_float64_mat_ = Unsafe_read_c.bin_read_float64_mat
let bin_read_float64_mat__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "float64_mat"

let bin_write_vec_ = Unsafe_write_c.bin_write_vec
let bin_read_vec_ = Unsafe_read_c.bin_read_vec
let bin_read_vec__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "vec"

let bin_write_float32_vec_ = Unsafe_write_c.bin_write_float32_vec
let bin_read_float32_vec_ = Unsafe_read_c.bin_read_float32_vec
let bin_read_float32_vec__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "float32_vec"

let bin_write_float64_vec_ = Unsafe_write_c.bin_write_float64_vec
let bin_read_float64_vec_ = Unsafe_read_c.bin_read_float64_vec
let bin_read_float64_vec__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "float64_vec"
