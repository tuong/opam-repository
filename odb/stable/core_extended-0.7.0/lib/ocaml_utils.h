#ifndef OCAML_UTILS_H
#define OCAML_UTILS_H

#include "jane_common.h"

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>

#define Nothing ((value) 0)
#define XSTR(S) STR(S)
#define STR(S) #S

extern void unix_error (int errcode, char *cmdname, value arg) Noreturn;
extern value unix_error_of_code(int errcode);
extern void uerror (char *cmdname, value arg) Noreturn;

extern value getsockopt_int(int *tcpopt, value sock, int level, value option);

extern value setsockopt_int(
  int *tcpopt, value sock, int level, value option, value status);

extern int caml_convert_signal_number(int signo);
extern int caml_rev_convert_signal_number(int signo);

extern void raise_with_two_args(value tag, value arg1, value arg2) Noreturn;

#endif /* OCAML_UTILS_H */
