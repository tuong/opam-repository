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

(* Binable: signatures defining generated functions for the binary protocol *)

module type S = sig
  type binable

  val bin_size_t : binable Size.sizer
  val bin_write_t : binable Map_to_safe.writer
  val bin_write_t_ : binable Unsafe_write_c.writer
  val bin_read_t : binable Read_ml.reader
  val bin_read_t_ : binable Unsafe_read_c.reader
  val bin_read_t__ : (int -> binable) Unsafe_read_c.reader
  val bin_writer_t : binable Type_class.writer
  val bin_reader_t : binable Type_class.reader
  val bin_t : binable Type_class.t
end

module type S1 = sig
  type 'a binable

  val bin_size_t : ('a, 'a binable) Size.sizer1
  val bin_write_t :('a, 'a binable) Map_to_safe.writer1
  val bin_write_t_ :('a, 'a binable) Unsafe_write_c.writer1
  val bin_read_t : ('a, 'a binable) Map_to_safe.reader1
  val bin_read_t_ : ('a, 'a binable) Unsafe_read_c.reader1
  val bin_read_t__ : ('a, int -> 'a binable) Unsafe_read_c.reader1
  val bin_writer_t : ('a, 'a binable) Type_class.S1.writer
  val bin_reader_t : ('a, 'a binable) Type_class.S1.reader
  val bin_t : ('a, 'a binable) Type_class.S1.t
end

module type S2 = sig
  type ('a, 'b) binable

  val bin_size_t : ('a, 'b, ('a, 'b) binable) Size.sizer2
  val bin_write_t :('a, 'b, ('a, 'b) binable) Map_to_safe.writer2
  val bin_write_t_ :('a, 'b, ('a, 'b) binable) Unsafe_write_c.writer2
  val bin_read_t : ('a, 'b, ('a, 'b) binable) Map_to_safe.reader2
  val bin_read_t_ : ('a, 'b, ('a, 'b) binable) Unsafe_read_c.reader2
  val bin_read_t__ : ('a, 'b, int -> ('a, 'b) binable) Unsafe_read_c.reader2
  val bin_writer_t : ('a, 'b, ('a, 'b) binable) Type_class.S2.writer
  val bin_reader_t : ('a, 'b, ('a, 'b) binable) Type_class.S2.reader
  val bin_t : ('a, 'b, ('a, 'b) binable) Type_class.S2.t
end
