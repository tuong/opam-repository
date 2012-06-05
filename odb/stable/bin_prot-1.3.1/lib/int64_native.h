/******************************************************************************
 *                             Bin-prot                                       *
 *                                                                            *
 * Copyright (C) 2008- Jane Street Holding, LLC                               *
 * Copyright (C) 2002 - INRIA                                                 *
 *    Contact: opensource@janestreet.com                                      *
 *    WWW: http://www.janestreet.com/ocaml                                    *
 *    Author: Markus Mottl, Xavier Leroy                                      *
 *                                                                            *
 * This file is derived from source code of the Ocaml compiler.               *
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
 ******************************************************************************/

/* Wrapper macros around native 64-bit integer arithmetic,
   so that it has the same interface as the software emulation
   provided in int64_emul.h */

#ifndef CAML_INT64_NATIVE_H
#define CAML_INT64_NATIVE_H

#define I64_literal(hi,lo) ((int64)(hi) << 32 | (lo))
#define I64_compare(x,y) (((x) > (y)) - ((x) < (y)))
#define I64_ult(x,y) ((uint64)(x) < (uint64)(y))
#define I64_neg(x) (-(x))
#define I64_add(x,y) ((x) + (y))
#define I64_sub(x,y) ((x) - (y))
#define I64_mul(x,y) ((x) * (y))
#define I64_is_zero(x) ((x) == 0)
#define I64_is_negative(x) ((x) < 0)
#define I64_div(x,y) ((x) / (y))
#define I64_mod(x,y) ((x) % (y))
#define I64_udivmod(x,y,quo,rem) \
  (*(rem) = (uint64)(x) % (uint64)(y), \
   *(quo) = (uint64)(x) / (uint64)(y))
#define I64_and(x,y) ((x) & (y))
#define I64_or(x,y) ((x) | (y))
#define I64_xor(x,y) ((x) ^ (y))
#define I64_lsl(x,y) ((x) << (y))
#define I64_asr(x,y) ((x) >> (y))
#define I64_lsr(x,y) ((uint64)(x) >> (y))
#define I64_to_intnat(x) ((intnat) (x))
#define I64_of_intnat(x) ((intnat) (x))
#define I64_to_int32(x) ((int32) (x))
#define I64_of_int32(x) ((int64) (x))
#define I64_to_double(x) ((double)(x))
#define I64_of_double(x) ((int64)(x))

#endif /* CAML_INT64_NATIVE_H */
