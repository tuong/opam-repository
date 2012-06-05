/******************************************************************************
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
 ******************************************************************************/

#include <string.h>
#include <unistd.h>
#include <errno.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/bigarray.h>

/* Utility definitions */

static inline char * get_bstr(value v_bstr, value v_pos)
{
  return (char *) Caml_ba_data_val(v_bstr) + Int_val(v_pos);
}


/* Marshalling to/from bigstrings */

extern CAMLprim int
caml_output_value_to_block(value v, value v_flags, char *bstr, int len);

CAMLprim value bigstring_marshal_blit_stub(
  value v, value v_pos, value v_len, value v_bstr, value v_flags)
{
  char *bstr = get_bstr(v_bstr, v_pos);
  return Val_int(caml_output_value_to_block(v, v_flags, bstr, Int_val(v_len)));
}

extern CAMLprim void
caml_output_value_to_malloc(value v, value v_flags, char **buf_p, long *len);

CAMLprim value bigstring_marshal_stub(value v, value v_flags)
{
  char *buf;
  long len;
  int alloc_flags = CAML_BA_UINT8 | CAML_BA_C_LAYOUT | CAML_BA_MANAGED;
  caml_output_value_to_malloc(v, v_flags, &buf, &len);
  return caml_ba_alloc(alloc_flags, 1, buf, &len);
}

extern CAMLprim value caml_marshal_data_size(value v_str, value v_pos);

CAMLprim value bigstring_marshal_data_size_stub(value v_pos, value v_bstr)
{
  CAMLparam1(v_bstr);
  value v_str = (value) Caml_ba_data_val(v_bstr);
  value v_res = caml_marshal_data_size(v_str, v_pos);
  CAMLreturn(v_res);
}

extern CAMLprim value caml_input_value_from_block(char *buff, int len);

CAMLprim value bigstring_unmarshal_stub(value v_pos, value v_len, value v_bstr)
{
  CAMLparam1(v_bstr);
  char *bstr = get_bstr(v_bstr, v_pos);
  value v_res = caml_input_value_from_block(bstr, Int_val(v_len));
  CAMLreturn(v_res);
}
