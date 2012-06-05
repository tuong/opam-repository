/******************************************************************************
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
 ******************************************************************************/

/* Common binary protocol definitions */

#ifndef COMMON_STUBS_H
#define COMMON_STUBS_H

#include <string.h>
#include <arpa/inet.h>

#include <caml/config.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/signals.h>


/* Portable byte swapping */

#ifndef bswap_16
#define bswap_16(value) \
  (((uint16_t) ((value) & 0xff) << 8) | ((uint16_t) (value) >> 8))
#endif

#ifndef bswap_32
#define bswap_32(value) \
  (((uint32_t) bswap_16(((value) & 0xffff)) << 16) | \
    (uint32_t) bswap_16(((value) >> 16)))
#endif

#ifndef bswap_64
#define bswap_64(value) \
  (((uint64_t) bswap_32(((value) & 0xffffffff)) << 32) | \
    (uint64_t) bswap_32(((value) >> 32)))
#endif


/* Bin-prot integer codes */

#define CODE_NEG_INT8 (char) -1
#define CODE_INT16 (char) -2
#define CODE_INT32 (char) -3
#define CODE_INT64 (char) -4


/* Buffer short exception */

extern value *v_bin_prot_exc_Buffer_short;


/* GNU compiler pragmas */

#if __GNUC__ >= 3
# ifndef inline
#   define inline inline __attribute__ ((always_inline))
# endif
# ifndef __pure
#   define __pure __attribute__ ((pure))
# endif
# ifndef __const
#   define __const __attribute__ ((const))
# endif
# ifndef __malloc
#   define __malloc __attribute__ ((malloc))
# endif
# ifndef __unused
#   define __unused __attribute__ ((unused))
# endif
# ifndef __likely
#   define likely(x) __builtin_expect (!!(x), 1)
# endif
# ifndef __unlikely
#   define unlikely(x) __builtin_expect (!!(x), 0)
# endif
#else
# ifndef inline
#   define inline
# endif
# ifndef __pure
#   define __pure
# endif
# ifndef  __const
#   define __const
# endif
# ifndef  __malloc
#   define __malloc
# endif
# ifndef  __unused
#   define __unused
# endif
# ifndef  __likely
#   define likely(x) (x)
# endif
# ifndef  __unlikely
#   define unlikely(x) (x)
# endif
#endif

#endif /* COMMON_STUBS_H */
