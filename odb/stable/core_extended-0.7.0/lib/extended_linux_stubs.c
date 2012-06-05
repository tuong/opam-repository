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

#define _GNU_SOURCE

#include <unistd.h>
#include <fcntl.h>
#include <sys/epoll.h>

#include "ocaml_utils.h"
#include "unix_utils.h"

/* resuid */

CAMLprim value linux_getresuid_stub(value __unused v_unit)
{
  value v_res;
  uid_t ruid,euid,suid;

  if (getresuid(&ruid, &euid, &suid) == -1) uerror("getresuid", Nothing);

  v_res = caml_alloc_small(3, 0);
  Field(v_res, 0) = Val_int(ruid);
  Field(v_res, 1) = Val_int(euid);
  Field(v_res, 2) = Val_int(suid);

  return v_res;
}

CAMLprim value linux_setresuid_stub(value v_ruid, value v_euid, value v_suid)
{
  if (setresuid(Int_val(v_ruid), Int_val(v_euid), Int_val(v_suid)) == -1)
    uerror("setresuid", Nothing);

  return Val_unit;
}


/* Epoll */
/* Epoll functions where removed after 80221ddbe375
   HG cat this file at this revision if you need to resuscitate them
 */
/* Splicing - zero-copies between kernel buffers */

CAMLprim value linux_splice_make_flags_stub(value v_flags)
{
  int flags = 0, i = Wosize_val(v_flags);
  while (--i >= 0) {
    switch (Int_val(Field(v_flags, i))) {
      case 0 : flags |= SPLICE_F_MOVE; break;
      case 1 : flags |= SPLICE_F_NONBLOCK; break;
      case 2 : flags |= SPLICE_F_MORE; break;
      default : flags |= SPLICE_F_GIFT; break;
    }
  }
  return caml_copy_int32(flags);
}

CAMLprim value linux_splice_stub(
  value v_assume_fd_is_nonblocking,
  value v_fd_in, value v_off_in,
  value v_fd_out, value v_off_out,
  value v_len, value v_flags)
{
  int assume_fd_is_nonblocking = Bool_val(v_assume_fd_is_nonblocking);
  int fd_in = Int_val(v_fd_in);
  int fd_out = Int_val(v_fd_out);
  off64_t off_in = Long_val(v_off_in);
  off64_t *off_in_p = (off_in < 0) ? NULL : &off_in;
  off64_t off_out = Long_val(v_off_out);
  off64_t *off_out_p = (off_out < 0) ? NULL : &off_out;
  size_t len = Long_val(v_len);
  unsigned int flags = Int32_val(v_flags);
  long ret;
  value v_res;

  if (assume_fd_is_nonblocking)
    ret = splice(fd_in, off_in_p, fd_out, off_out_p, len, flags);
  else {
    caml_enter_blocking_section();
      ret = splice(fd_in, off_in_p, fd_out, off_out_p, len, flags);
    caml_leave_blocking_section();
  }

  if (ret == -1) uerror("splice", Nothing);

  v_res = caml_alloc_small(3, 0);
  Field(v_res, 0) = Val_long(ret);
  Field(v_res, 1) = Val_long(off_in);
  Field(v_res, 2) = Val_long(off_out);

  return v_res;
}

CAMLprim value linux_splice_stub_bc(value *argv, int __unused argn)
{
  return linux_splice_stub(
    argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}

CAMLprim value linux_tee_stub(
  value v_assume_fd_is_nonblocking,
  value v_fd_in, value v_fd_out,
  value v_len, value v_flags)
{
  int assume_fd_is_nonblocking = Bool_val(v_assume_fd_is_nonblocking);
  int fd_in = Int_val(v_fd_in);
  int fd_out = Int_val(v_fd_out);
  size_t len = Long_val(v_len);
  unsigned int flags = Int32_val(v_flags);
  long ret;

  if (assume_fd_is_nonblocking)
    ret = tee(fd_in, fd_out, len, flags);
  else {
    caml_enter_blocking_section();
      ret = tee(fd_in, fd_out, len, flags);
    caml_leave_blocking_section();
  }

  if (ret == -1) uerror("tee", Nothing);

  return Val_long(ret);
}

CAMLprim value linux_vmsplice_stub(
  value v_assume_fd_is_nonblocking,
  value v_fd, value v_iovecs, value v_count,
  value v_flags)
{
  int assume_fd_is_nonblocking = Bool_val(v_assume_fd_is_nonblocking);
  int fd = Int_val(v_fd);
  int count = Int_val(v_count);
  size_t total_len = 0;
  struct iovec *iovecs = copy_iovecs(&total_len, v_iovecs, count);
  unsigned int flags = Int32_val(v_flags);
  long ret;

  if (assume_fd_is_nonblocking && total_len < THREAD_IO_CUTOFF)
    ret = vmsplice(fd, iovecs, count, flags);
  else {
    Begin_roots1(v_iovecs);
    caml_enter_blocking_section();
      ret = vmsplice(fd, iovecs, count, flags);
    caml_leave_blocking_section();
    End_roots();
  }

  if (ret == -1) uerror("vmsplice", Nothing);

  return Val_long(ret);
}
