/* ocamlgsl - OCaml interface to GSL                        */
/* Copyright (©) 2002-2005 - Olivier Andrieu                */
/* distributed under the terms of the GPL version 2         */

#include <gsl/gsl_errno.h>
#include <gsl/gsl_version.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/fail.h>

CAMLprim value ml_gsl_version(value unit)
{
  return copy_string(gsl_version);
}

CAMLprim value ml_gsl_strerror(value ml_errno)
{
  int c_errno = Int_val(ml_errno);
  int gsl_errno = (c_errno <= 1) ? (c_errno - 2) : (c_errno - 1) ;
  return copy_string(gsl_strerror(gsl_errno));
}

static inline int conv_err_code(int gsl_errno)
{
  if(gsl_errno < 0)
    return gsl_errno + 2 ;
  else
    return gsl_errno + 1 ;
}

static value       *ml_gsl_exn;

static void ml_gsl_raise_exn(const char *msg, int gsl_errno)
{
  CAMLlocal2(exn_msg, exn_arg);
  exn_msg = copy_string(msg);
  exn_arg = alloc_small(2, 0);
  Field(exn_arg, 0) = Val_int(conv_err_code(gsl_errno));
  Field(exn_arg, 1) = exn_msg;
  if(ml_gsl_exn != NULL)
    raise_with_arg(*ml_gsl_exn, exn_arg);
  else
    failwith("GSL error");
}

static void ml_gsl_error_handler(const char *reason, const char *file,
				 int line, int gsl_errno)
{
  ml_gsl_raise_exn(reason, gsl_errno);
}

CAMLprim value ml_gsl_error_init(value init)
{
  static gsl_error_handler_t *old;
  if(ml_gsl_exn == NULL) 
    ml_gsl_exn = caml_named_value("mlgsl_exn");
  if(Bool_val(init))
    old = gsl_set_error_handler(&ml_gsl_error_handler);
  else
    gsl_set_error_handler(old);
  return Val_unit;
}
