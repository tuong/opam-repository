/******************************************************************************
 *                             Core                                           *
 *                                                                            *
 * Copyright (C) 2008- Jane Street Holding, LLC                               *
 *    Contact: opensource@janestreet.com                                      *
 *    WWW: http://www.janestreet.com/ocaml                                    *
 *                                                                            *
 *                                                                            *
 * This file is derived from source code of the Ocaml compiler.               *
 * which has additional copyrights:                                           *
 *                                                                            *
 *    Xavier Leroy, projet Cristal, INRIA Rocquencourt                        *
 *                                                                            *
 *    Copyright 1996 Institut National de Recherche en Informatique et        *
 *    en Automatique.                                                         *
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

#include <caml/misc.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <arpa/inet.h>

union sock_addr_union {
  struct sockaddr s_gen;
  struct sockaddr_un s_unix;
  struct sockaddr_in s_inet;
#ifdef HAS_IPV6
  struct sockaddr_in6 s_inet6;
#endif
};

#ifdef HAS_SOCKLEN_T
typedef socklen_t socklen_param_type;
#else
typedef int socklen_param_type;
#endif

extern void get_sockaddr (value mladdr,
                          union sock_addr_union * addr /*out*/,
                          socklen_param_type * addr_len /*out*/);
CAMLexport value alloc_sockaddr (union sock_addr_union * addr /*in*/,
                      socklen_param_type addr_len, int close_on_error);
CAMLexport value alloc_inet_addr (struct in_addr * inaddr);
#define GET_INET_ADDR(v) (*((struct in_addr *) (v)))

#ifdef HAS_IPV6
CAMLexport value alloc_inet6_addr (struct in6_addr * inaddr);
#define GET_INET6_ADDR(v) (*((struct in6_addr *) (v)))
#endif
