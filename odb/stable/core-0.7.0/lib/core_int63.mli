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

(** The size of Int63 is always at least 63 bits.  On a 64-bit
 * platform it is just an int (63-bits), and on a 32-bit platform it is an int64.
 *
 * Because Int63 has different sizes on 32-bit and 64-bit platforms, there are
 * several pitfalls to be aware of:
 *
 *   - Int63 will behave differently in the case of overflow.
 *   - marshalling Int63 will not work between 32-bit and 64-bit platforms.
 *     unmarshal will segfault.
 *   - bin_io will work, except that it will raise an overflow exception when
 *     you send too large of an int from a 32-bit to a 64-bit platform.  This
 *     is couterintuitive because the 32-bit platform has the larger int size.
 *)

include Int_intf.S

val of_int : int -> t
val to_int : t -> int option
