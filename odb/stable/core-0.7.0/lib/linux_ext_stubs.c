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

#define _FILE_OFFSET_BITS 64
#define _GNU_SOURCE

#include <string.h>
#include <unistd.h>
#include <sys/prctl.h>
#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <time.h>
#include <sched.h>
#include <termios.h>
#include <sys/ioctl.h>

#include <sys/sysinfo.h>

#include "ocaml_utils.h"
#include "unix_utils.h"

#define DIR_Val(v) *((DIR **) &Field(v, 0))
#define UNIX_BUFFER_SIZE 16384

#include <sys/sendfile.h>

CAMLprim value
linux_sendfile_stub(value v_sock, value v_fd, value v_pos, value v_len)
{
  loff_t pos = Long_val(v_pos);
  ssize_t ret;

  caml_enter_blocking_section();
    ret = sendfile(Int_val(v_sock), Int_val(v_fd), &pos, Long_val(v_len));
  caml_leave_blocking_section();

  if (ret == -1) uerror("sendfile", Nothing);

  return Val_long(ret);
}

CAMLprim value linux_sysinfo(value __unused v_unit)
{
  struct sysinfo s_info;
  int ret = sysinfo(&s_info);
  if (ret == -1) uerror("sysinfo", Nothing);
  else {
    value v_res = caml_alloc_small(14, 0);
    Field(v_res, 0) = Val_long(s_info.uptime);
    Field(v_res, 1) = Val_long(s_info.loads[0]);
    Field(v_res, 2) = Val_long(s_info.loads[1]);
    Field(v_res, 3) = Val_long(s_info.loads[2]);
    Field(v_res, 4) = Val_long(s_info.totalram);
    Field(v_res, 5) = Val_long(s_info.freeram);
    Field(v_res, 6) = Val_long(s_info.sharedram);
    Field(v_res, 7) = Val_long(s_info.bufferram);
    Field(v_res, 8) = Val_long(s_info.totalswap);
    Field(v_res, 9) = Val_long(s_info.freeswap);
    Field(v_res, 10) = Val_int(s_info.procs);
    Field(v_res, 11) = Val_long(s_info.totalhigh);
    Field(v_res, 12) = Val_long(s_info.freehigh);
    Field(v_res, 13) = Val_int(s_info.mem_unit);
    return v_res;
  }
}

/**/

static int linux_tcpopt_bool[] = { TCP_CORK };

enum option_type {
  TYPE_BOOL = 0,
  TYPE_INT = 1,
  TYPE_LINGER = 2,
  TYPE_TIMEVAL = 3,
  TYPE_UNIX_ERROR = 4
};

extern value unix_getsockopt_aux(
  char *name,
  enum option_type ty, int level, int option,
  value v_socket);
extern value unix_setsockopt_aux(
  char *name,
  enum option_type ty, int level, int option,
  value v_socket, value v_status);

CAMLprim value linux_gettcpopt_bool_stub(value v_socket, value v_option)
{
  int option = linux_tcpopt_bool[Int_val(v_option)];
  return
    unix_getsockopt_aux("getsockopt", TYPE_BOOL, SOL_TCP, option, v_socket);
}

CAMLprim value
linux_settcpopt_bool_stub(value v_socket, value v_option, value v_status)
{
  int option = linux_tcpopt_bool[Int_val(v_option)];
  return
    unix_setsockopt_aux(
      "setsockopt", TYPE_BOOL, SOL_TCP, option, v_socket, v_status);
}

/**/

static int nonblocking_no_sigpipe_flag = MSG_DONTWAIT | MSG_NOSIGNAL;

CAMLprim value linux_send_nonblocking_no_sigpipe_stub(
  value v_fd, value v_pos, value v_len, value v_buf)
{
  char *buf = String_val(v_buf) + Long_val(v_pos);
  ssize_t ret =
    send(Int_val(v_fd), buf, Long_val(v_len), nonblocking_no_sigpipe_flag);
  if (ret == -1 && errno != EAGAIN && errno != EWOULDBLOCK)
    uerror("send_nonblocking_no_sigpipe", Nothing);
  return Val_long(ret);
}

CAMLprim value linux_send_no_sigpipe_stub(
  value v_fd, value v_pos, value v_len, value v_buf)
{
  char *buf = String_val(v_buf) + Long_val(v_pos);
  ssize_t ret = send(Int_val(v_fd), buf, Long_val(v_len), MSG_NOSIGNAL);
  if (ret == -1) uerror("send_no_sigpipe", Nothing);
  return Val_long(ret);
}

CAMLprim value linux_sendmsg_nonblocking_no_sigpipe_stub(
  value v_fd, value v_iovecs, value v_count)
{
  int count = Int_val(v_count);
  ssize_t ret;
  struct iovec *iovecs = caml_stat_alloc(sizeof(struct iovec) * count);
  struct msghdr msghdr = { NULL, 0, NULL, 0, NULL, 0, 0 };
  msghdr.msg_iov = iovecs;
  msghdr.msg_iovlen = count;
  for (--count; count >= 0; --count) {
    struct iovec *iovec = &iovecs[count];
    value v_iovec = Field(v_iovecs, count);
    value v_iov_base = Field(v_iovec, 0);
    value v_iov_pos = Field(v_iovec, 1);
    value v_iov_len = Field(v_iovec, 2);
    iovec->iov_base = String_val(v_iov_base) + Long_val(v_iov_pos);
    iovec->iov_len = Long_val(v_iov_len);
  }
  ret = sendmsg(Int_val(v_fd), &msghdr, nonblocking_no_sigpipe_flag);
  caml_stat_free(iovecs);
  if (ret == -1 && errno != EAGAIN && errno != EWOULDBLOCK)
    uerror("sendmsg_nonblocking_no_sigpipe", Nothing);
  return Val_long(ret);
}

CAMLprim value linux_pr_set_pdeathsig_stub(value v_sig)
{
  int sig = caml_convert_signal_number(Int_val(v_sig));
  if (prctl(PR_SET_PDEATHSIG, sig) == -1) uerror("pr_set_pdeathsig", Nothing);
  return Val_unit;
}

CAMLprim value linux_pr_get_pdeathsig_stub(value __unused v_unit)
{
  int sig;
  if (prctl(PR_GET_PDEATHSIG, &sig) == -1) uerror("pr_get_pdeathsig", Nothing);
  return Val_int(caml_rev_convert_signal_number(sig));
}

static void cpulist_to_cpuset(value cpulist, cpu_set_t * cpuset)
{
  value l;
  CPU_ZERO(cpuset);
  for (l = cpulist; l != Val_int(0); l = Field(l, 1)) {
    int cpu = Int_val(Field(l, 0));
    CPU_SET(cpu, cpuset);
  }
}

CAMLprim value linux_sched_setaffinity(value v_pid, value cpulist)
{
  cpu_set_t set;
  pid_t pid;
  pid = Int_val(v_pid);
  cpulist_to_cpuset(cpulist, &set);
  if (sched_setaffinity(pid, sizeof(cpu_set_t), &set) != 0)
    uerror("setaffinity", Nothing);
  return Val_unit;
}

CAMLprim value linux_pr_set_name(value v_name)
{
  char *buf = String_val(v_name);
  if (prctl(PR_SET_NAME, (unsigned long) buf) == -1)
    uerror("pr_set_name", Nothing);
  return Val_unit;
}

CAMLprim value linux_pr_get_name(value __unused v_unit)
{
  char buf[17];

  buf[16] = 0;
  if (prctl(PR_GET_NAME, (unsigned long) buf) == -1)
    uerror("pr_get_name", Nothing);
  return caml_copy_string(buf);
}
