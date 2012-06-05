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
