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

/* Core_unix support functions written in C. */

#include <unistd.h>
#include <stdlib.h>
#include <signal.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <caml/mlvalues.h>
#include "ocaml_utils.h"

#define MAX_ERROR_LEN 4096

/* Copy an ocaml string array in a c string array
   the result need to be free'd with a stat_free
   It is a copy of cstringvect in the ocaml unix's module.
 */
char ** copy_stringvect(value arg)
{
  char ** res;
  mlsize_t size, i;

  size = Wosize_val(arg);
  res = (char **) caml_stat_alloc((size + 1) * sizeof(char *));
  for (i = 0; i < size; i++) res[i] = String_val(Field(arg, i));
  res[size] = NULL;
  return res;
}

/*
   Report errors on the forked side and then exits with 254 (255 clashes with
   ssh)
*/
static void forked__report_error(int fd, const char* str)
{
  char buf[MAX_ERROR_LEN];
  char buf2[MAX_ERROR_LEN];
  if (strerror_r(errno, buf, MAX_ERROR_LEN) == -1)
    snprintf(buf, 80, "Unknown error %d", errno);
  snprintf(buf2, MAX_ERROR_LEN, "%s (%s)\n", str, buf);
  buf2[MAX_ERROR_LEN - 1] = '\0';
  write(fd, buf2, strlen(buf2));
  fsync(2);
  /* we use __exit instead of exit because the child shouldn't
     do any more cleaning (e.g. flushing). */
  _exit(254);
}

/* Close function that handles signals correctly by retrying the close
   after EINTR.

   NOTE: we should never see EIO when closing pipes.  If so, it is
   reasonable to see this as a kernel bug, and it's pretty useless trying
   to catch/work around potential kernel bugs.  We assume that it works.
   An EBADF would be bad when closing successfully opened pipes, too, but
   in that case the pipe should be guaranteed to be closed anyway (unlike
   EIO).  This covers all errors that close could potentially return.
*/
static inline void forked__close(int fd, int err_fd)
{
  int ret;
  while ((ret = close(fd)) == -1 && errno == EINTR) /* empty loop */ ;
  if (ret == -1) forked__report_error(err_fd ,"Failed to close fds");
  return;
}

static inline void forked__dup2(int src, int tgt, int err_fd)
{
  int ret;
  if (src != tgt) {
    while ((ret = dup2 (src, tgt)) == -1 && errno == EINTR) /* empty loop */;
    if (ret == -1) forked__report_error(err_fd,"Failed to dup2");
  }
}

/*
  Tries to dup and report to err_fd in case it fails.
*/
static inline int forked__dup(int fd, int err_fd){
  int ret;
  while ((ret = dup(fd)) == -1 && errno == EINTR) /* empty loop */ ;
  if (ret == -1)
    forked__report_error(err_fd, "could not dup fds in child process");
  return ret;
}

/*
  Unblock all the signals...
 */
void unblock_sigs (void)
{
  sigset_t empty;
  sigemptyset (&empty);
  sigprocmask (2, &empty, (sigset_t *) 0);
}

#ifdef __APPLE__
# include <crt_externs.h>
int clearenv (void)
{
  *_NSGetEnviron() = NULL;
  return 0;
}
#endif

/* Given v_prog, an O'Caml string value specifying a program name,
   v_args, an O'Caml array specifying program arguments (not
   including the program name), and v_search_path, an O'Caml boolean
   value specifying whether to search the PATH, fork a child process
   that executes the specified program.  Return the child's pid together
   with fds connected via pipes to the stdin, stdout and stderr of the
   program such that if the fds are closed the pipes are broken.

   Beware!
   A great deal of work has gone into making this subtle function thoroughly
   robust and, hopefully, correct.  Changes should not be undertaken lightly.
*/

CAMLprim value extended_ml_create_process
(value v_prog,
 value v_args, /* Full list of args passed to executable... */
 value v_stdin,
 value v_stdout,
 value v_stderr,
 value v_fds_to_close,
 value v_clear_env,
 value v_setenv_pairs)
{
  CAMLparam5(v_prog, v_args, v_stdin, v_stdout, v_stderr);
  CAMLxparam3(v_fds_to_close,v_clear_env,v_setenv_pairs);
  int stdin_fd = Int_val (v_stdin);
  int stdout_fd = Int_val (v_stdout);
  int stderr_fd = Int_val (v_stderr);
  int should_clearenv = Bool_val(v_clear_env);
  value setenv_pair;
  int i;

  /* It's ok to hold pointers into the O'Caml heap, since the memory
     space gets duplicated upon the fork, during which we keep the
     O'Caml lock. */
  char* prog = String_val(v_prog);

  pid_t child_pid;

  char** args = copy_stringvect(v_args);
  /* This function deliberately doesn't release the O'Caml lock (i.e. it
     doesn't call caml_enter_blocking_section) during the fork.  This is
     because we hold pointers into the ML heap across a fork, and
     releasing the lock immediately before the fork could theoretically
     cause the GC to run and move blocks before the fork duplicates the
     memory space.

     If the parent process has threads that turn out to suffer from too
     much latency during this fork, we may want to rewrite this function
     to copy the O'Caml values into the C heap before the fork and release
     the O'Caml lock.  It seems unlikely that forks will ever take so
     long that people care.  In Linux 2.6 forks are practically constant
     time even in the presence of ridiculous amounts of processes, and
     are reported to always have less than 500us latency.  Maybe the
     kernel does not even schedule threads during forks anyway.  */
  switch (child_pid = vfork()) {
  case -1:
    caml_stat_free(args);
    uerror("unsafe_process: fork failed", Nothing);
  case 0:
    unblock_sigs();
    /* Child process. */

    /* Just in case any of the pipes' file descriptors are 0, 1 or 2
       (not inconceivable, especially when running as a daemon),
       duplicate all three descriptors we need in the child to fresh
       descriptors before duplicating them onto stdin, stdout and stderr.

       This will ensure that there is one and only one copy of the file
       descriptors passed as arguments with id's higher than 2.
    */
    while (stdin_fd <= 2) stdin_fd = forked__dup(stdin_fd, stderr_fd);
    while (stdout_fd <= 2) stdout_fd = forked__dup(stdout_fd, stderr_fd);
    while (stderr_fd <= 2) stderr_fd = forked__dup(stderr_fd, stderr_fd);

    /* Close all the descriptors we want to get rid of. */
    for (i = Wosize_val(v_fds_to_close) - 1;i >= 0;i--)
      forked__close(Int_val(Field(v_fds_to_close, i)),stderr_fd);

    /* Set up the new environement; we set things up here instead of passing an
       environement to exec because setenv and clearenv are more standard. */
    if (should_clearenv)
      if (clearenv())
        forked__report_error(stderr_fd,"failed to clearenv in child process");

    for (i = Wosize_val(v_setenv_pairs) - 1; i >= 0 ;i--) {
      setenv_pair = Field(v_setenv_pairs, i);
      if (setenv (String_val(Field(setenv_pair, 0)),
                  String_val(Field(setenv_pair, 1)),
                  1))
        forked__report_error(stderr_fd, "setenv failed in child process");
    }

    /* We must dup2 the descriptors back in place... */
    forked__dup2(stdin_fd, 0, stderr_fd);
    forked__dup2(stdout_fd, 1, stderr_fd);
    forked__dup2(stderr_fd, 2, stderr_fd);

    /* And close the ones we had moved out of the way... */
    forked__close(stdin_fd, stderr_fd);
    forked__close(stdout_fd, stderr_fd);
    forked__close(stderr_fd, 2);

    /* path lookups should be done on the parent side of the fork */
    execv(prog, args);
    forked__report_error(2, "execvp/execv failed in child process");

  default : /* Parent process */
    caml_stat_free(args);
    CAMLreturn(Val_int(child_pid));
  }
}

CAMLprim value extended_ml_create_process_bc(value *argv, int argn)
{
  if (argn != 8) {
    caml_failwith("Unix.ml_create_process_bc got the wrong number of \
     arguments. This is due to an error in the FFI.");
  }
  return
    extended_ml_create_process(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6],
      argv[7]);
}
