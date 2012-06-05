/******************************************************************************
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
 ******************************************************************************/

#include <malloc.h>
#include "ocaml_utils.h"

CAMLprim value malloc_mallinfo_stub(value __unused v_unit)
{
  struct mallinfo info = mallinfo();
  value v_info = caml_alloc_small(10, 0);
  Field(v_info, 0) = Val_int(info.arena);
  Field(v_info, 1) = Val_int(info.ordblks);
  Field(v_info, 2) = Val_int(info.smblks);
  Field(v_info, 3) = Val_int(info.hblks);
  Field(v_info, 4) = Val_int(info.hblkhd);
  Field(v_info, 5) = Val_int(info.usmblks);
  Field(v_info, 6) = Val_int(info.fsmblks);
  Field(v_info, 7) = Val_int(info.uordblks);
  Field(v_info, 8) = Val_int(info.fordblks);
  Field(v_info, 9) = Val_int(info.keepcost);
  return v_info;
}

static int options[] = {
  M_TRIM_THRESHOLD,
  M_TOP_PAD,
  M_MMAP_THRESHOLD,
  M_MMAP_MAX,
  M_CHECK_ACTION,
/*  M_PERTURB, */
};

CAMLprim value malloc_mallopt_stub(value v_opt, value v_n)
{
  int ret = mallopt(options[Int_val(v_opt)], Int_val(v_n));
  if (ret != 1) caml_failwith("mallopt");
  return Val_unit;
}

CAMLprim value malloc_trim_stub(value v_n)
{
  int ret = malloc_trim(Int_val(v_n));
  if (ret != 1) caml_failwith("malloc_trim");
  return Val_unit;
}

CAMLprim value malloc_stats_stub(value __unused v_unit)
{
  caml_enter_blocking_section();
  malloc_stats();
  caml_leave_blocking_section();
  return Val_unit;
}
