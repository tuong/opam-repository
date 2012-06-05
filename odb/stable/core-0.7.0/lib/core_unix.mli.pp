(* This file is a copy of unixLabels.mli from the OCaml distribution.
   It has been modified to remove "external" specifications.
   There are many additions as well.
*)

(** Interface to the Unix system.
   To use as replacement to default {!Unix} module,
   add [module Unix = UnixLabels] in your implementation.
*)

(** File descriptor. *)
module File_descr : sig
  type t = Caml.Unix.file_descr
  include Hashable.S with type hashable = t
  include Sexpable.S with type sexpable = t
  val to_int : t -> int
  val to_string : t -> string
  val sexp_of_t : t -> Sexplib.Sexp.t
end

(** {6 Error report} *)

(** The type of error codes.
   Errors defined in the POSIX standard
   and additional errors, mostly BSD.
   All other errors are mapped to EUNKNOWNERR.
*)
type error =
  Unix.error =
      E2BIG               (** Argument list too long *)
    | EACCES              (** Permission denied *)
    | EAGAIN              (** Resource temporarily unavailable; try again *)
    | EBADF               (** Bad file descriptor *)
    | EBUSY               (** Resource unavailable *)
    | ECHILD              (** No child process *)
    | EDEADLK             (** Resource deadlock would occur *)
    | EDOM                (** Domain error for math functions, etc. *)
    | EEXIST              (** File exists *)
    | EFAULT              (** Bad address *)
    | EFBIG               (** File too large *)
    | EINTR               (** Function interrupted by signal *)
    | EINVAL              (** Invalid argument *)
    | EIO                 (** Hardware I/O error *)
    | EISDIR              (** Is a directory *)
    | EMFILE              (** Too many open files by the process *)
    | EMLINK              (** Too many links *)
    | ENAMETOOLONG        (** Filename too long *)
    | ENFILE              (** Too many open files in the system *)
    | ENODEV              (** No such device *)
    | ENOENT              (** No such file or directory *)
    | ENOEXEC             (** Not an executable file *)
    | ENOLCK              (** No locks available *)
    | ENOMEM              (** Not enough memory *)
    | ENOSPC              (** No space left on device *)
    | ENOSYS              (** Function not supported *)
    | ENOTDIR             (** Not a directory *)
    | ENOTEMPTY           (** Directory not empty *)
    | ENOTTY              (** Inappropriate I/O control operation *)
    | ENXIO               (** No such device or address *)
    | EPERM               (** Operation not permitted *)
    | EPIPE               (** Broken pipe *)
    | ERANGE              (** Result too large *)
    | EROFS               (** Read-only file system *)
    | ESPIPE              (** Invalid seek e.g. on a pipe *)
    | ESRCH               (** No such process *)
    | EXDEV               (** Invalid link *)

    | EWOULDBLOCK         (** Operation would block *)
    | EINPROGRESS         (** Operation now in progress *)
    | EALREADY            (** Operation already in progress *)
    | ENOTSOCK            (** Socket operation on non-socket *)
    | EDESTADDRREQ        (** Destination address required *)
    | EMSGSIZE            (** Message too long *)
    | EPROTOTYPE          (** Protocol wrong type for socket *)
    | ENOPROTOOPT         (** Protocol not available *)
    | EPROTONOSUPPORT     (** Protocol not supported *)
    | ESOCKTNOSUPPORT     (** Socket type not supported *)
    | EOPNOTSUPP          (** Operation not supported on socket *)
    | EPFNOSUPPORT        (** Protocol family not supported *)
    | EAFNOSUPPORT        (** Address family not supported by protocol family *)
    | EADDRINUSE          (** Address already in use *)
    | EADDRNOTAVAIL       (** Can't assign requested address *)
    | ENETDOWN            (** Network is down *)
    | ENETUNREACH         (** Network is unreachable *)
    | ENETRESET           (** Network dropped connection on reset *)
    | ECONNABORTED        (** Software caused connection abort *)
    | ECONNRESET          (** Connection reset by peer *)
    | ENOBUFS             (** No buffer space available *)
    | EISCONN             (** Socket is already connected *)
    | ENOTCONN            (** Socket is not connected *)
    | ESHUTDOWN           (** Can't send after socket shutdown *)
    | ETOOMANYREFS        (** Too many references: can't splice *)
    | ETIMEDOUT           (** Connection timed out *)
    | ECONNREFUSED        (** Connection refused *)
    | EHOSTDOWN           (** Host is down *)
    | EHOSTUNREACH        (** No route to host *)
    | ELOOP               (** Too many levels of symbolic links *)
    | EOVERFLOW           (** File size or position not representable *)

    | EUNKNOWNERR of int  (** Unknown error *)
with sexp


(** Raised by the system calls below when an error is encountered.
   The first component is the error code; the second component
   is the function name; the third component is the string parameter
   to the function, if it has one, or the empty string otherwise. *)
exception Unix_error of error * string * string

(** @raise Unix_error with a given errno, function name and argument *)
external unix_error : int -> string -> string -> _ = "unix_error_stub"

(** Return a string describing the given error code. *)
val error_message : error -> string


(** [handle_unix_error f x] applies [f] to [x] and returns the result.
   If the exception [Unix_error] is raised, it prints a message
   describing the error and exits with code 2. *)
val handle_unix_error : ('a -> 'b) -> 'a -> 'b

(** {6 Access to the process environment} *)

(** Return the process environment, as an array of strings
    with the format ``variable=value''. *)
val environment : unit -> string array



(** [Unix.putenv ~key ~data] sets the value associated to a
   variable in the process environment.
   [key] is the name of the environment variable,
   and [data] its new associated value. *)
val putenv : key : string -> data : string -> unit

(**
   [unsetenv name] deletes the variable [name] from the environment.

   EINVAL [name] contained an ’=’ or an '\000' character.
*)
val unsetenv : string -> unit

(** {6 Process handling} *)

(** The termination status of a process. *)
module Process_status : sig
  type t = [
  | `Exited of int
  | `Signaled of Signal.t
  | `Stopped of Signal.t
  ] with sexp

  val is_ok : t -> bool
  (* [of_unix] assumes that any signal numbers in the incoming value are
     O'Caml internal signal numbers. *)
  val of_unix : Caml.Unix.process_status -> t

  val to_string_hum : t -> string
end

(** Flags for {!UnixLabels.waitpid}. *)

type wait_flag =
  Unix.wait_flag =
    | WNOHANG   (** do not block if no child has
                    died yet, but immediately return with a pid equal to 0.*)
    | WUNTRACED (* report also the children that receive stop signals. *)

(** [execv prog args] execute the program in file [prog], with
   the arguments [args], and the current process environment.
   These [execv*] functions never return: on success, the current
   program is replaced by the new one;
   on failure, a {!UnixLabels.Unix_error} exception is raised. *)

val execv : prog:string -> args:string array -> _

(** Same as {!UnixLabels.execv}, except that the third argument provides the
   environment to the program executed. *)
val execve : prog:string -> args:string array -> env:string array -> _

(** Same as {!UnixLabels.execv} respectively, except that
   the program is searched in the path. *)
val execvp : prog:string -> args:string array -> _

(** Same as {!UnixLabels.execve} respectively, except that
   the program is searched in the path. *)
val execvpe : prog:string -> args:string array -> env:string array -> _




(** Fork a new process. The returned integer is 0 for the child
   process, the pid of the child process for the parent process. *)
val fork : unit -> int

(** Wait until one of the children processes die, and return its pid
   and termination status. *)
val wait : ?restart:bool -> unit -> int * Process_status.t

(** Same as {!UnixLabels.wait}, but waits for the process whose pid is given.
   A pid of [-1] means wait for any child.
   A pid of [0] means wait for any child in the same process group
   as the current process.
   Negative pid arguments represent process groups.
   The list of options indicates whether [waitpid] should return
   immediately without waiting, or also report stopped children.
   [restart] (default false) causes the system call to be retried
   upon EINTR. *)


val waitpid : ?restart:bool -> mode:wait_flag list -> int
              -> int * Process_status.t

(** Execute the given command, wait until it terminates, and return
   its termination status. The string is interpreted by the shell
   [/bin/sh] and therefore can contain redirections, quotes, variables,
   etc. The result [WEXITED 127] indicates that the shell couldn't
   be executed. *)
val system : string -> Process_status.t

(** Return the pid of the process. *)
val getpid : unit -> int

(** Return the pid of the parent process. *)
val getppid : unit -> int

(** Change the process priority. The integer argument is added to the
   ``nice'' value. (Higher values of the ``nice'' value mean
   lower priorities.) Return the new nice value. *)
val nice : int -> int


(** {6 Basic file input/output} *)


(** The abstract type of file descriptors. *)
type file_descr = Unix.file_descr

(** File descriptor for standard input.*)
val stdin : file_descr

(** File descriptor for standard output.*)
val stdout : file_descr

(** File descriptor for standard standard error. *)
val stderr : file_descr

(** The flags to {!UnixLabels.openfile}. *)
type open_flag =
  Unix.open_flag =
      O_RDONLY     (** Open for reading *)
    | O_WRONLY     (** Open for writing *)
    | O_RDWR       (** Open for reading and writing *)
    | O_NONBLOCK   (** Open in non-blocking mode *)
    | O_APPEND     (** Open for append *)
    | O_CREAT      (** Create if nonexistent *)
    | O_TRUNC      (** Truncate to 0 length if existing *)
    | O_EXCL       (** Fail if existing *)
    | O_NOCTTY     (** Don't make this dev a controlling tty *)
    | O_DSYNC      (** Writes complete as `Synchronised I/O data integrity completion' *)
    | O_SYNC       (** Writes complete as `Synchronised I/O file integrity completion' *)
    | O_RSYNC      (** Reads complete as writes (depending on O_SYNC/O_DSYNC) *)


(** The type of file access rights. *)
type file_perm = int with sexp



(** Open the named file with the given flags. Third argument is
   the permissions to give to the file if it is created. Return
   a file descriptor on the named file. *)
val openfile : string -> mode:open_flag list -> perm:file_perm -> file_descr

(** Close a file descriptor. *)
val close : file_descr -> unit

(** [read fd buff ofs len] reads [len] characters from descriptor
   [fd], storing them in string [buff], starting at position [ofs]
   in string [buff]. Return the number of characters actually read. *)
val read : file_descr -> buf:string -> pos:int -> len:int -> int

(** [write fd buff ofs len] writes [len] characters to descriptor
   [fd], taking them from string [buff], starting at position [ofs]
   in string [buff]. Return the number of characters actually
   written.

   When an error is reported some characters might have already been
   written.  Use [single_write] instead to ensure that this is not the
   case. *)
val write : file_descr -> buf:string -> pos:int -> len:int -> int

(** Same as [write] but ensures that all errors are reported and
   that no character has ever been written when an error is reported. *)
val single_write : file_descr -> buf:string -> pos:int -> len:int -> int


(** {6 Interfacing with the standard input/output library} *)


(** Create an input channel reading from the given descriptor.
   The channel is initially in binary mode; use
   [set_binary_mode_in ic false] if text mode is desired. *)
val in_channel_of_descr : file_descr -> in_channel

(** Create an output channel writing on the given descriptor.
   The channel is initially in binary mode; use
   [set_binary_mode_out oc false] if text mode is desired. *)
val out_channel_of_descr : file_descr -> out_channel

(** Return the descriptor corresponding to an input channel. *)
val descr_of_in_channel : in_channel -> file_descr

(** Return the descriptor corresponding to an output channel. *)
val descr_of_out_channel : out_channel -> file_descr


(** {6 Seeking and truncating} *)

(** Positioning modes for {!UnixLabels.lseek}. *)
#undef SEEK_SET
#undef SEEK_CUR
#undef SEEK_END
type seek_command =
  Unix.seek_command =
      SEEK_SET (** indicates positions relative to the beginning of the file *)
    | SEEK_CUR (** indicates positions relative to the current position *)
    | SEEK_END (** indicates positions relative to the end of the file *)

(** Set the current position for a file descriptor *)
val lseek : file_descr -> int64 -> mode:seek_command -> int64

(** Truncates the named file to the given size. *)
val truncate : string -> len:int64 -> unit

(** Truncates the file corresponding to the given descriptor
   to the given size. *)
val ftruncate : file_descr -> len:int64 -> unit



(** {6 File statistics} *)

type file_kind =
  Unix.file_kind =
      S_REG                   (** Regular file *)
    | S_DIR                   (** Directory *)
    | S_CHR                   (** Character device *)
    | S_BLK                   (** Block device *)
    | S_LNK                   (** Symbolic link *)
    | S_FIFO                  (** Named pipe *)
    | S_SOCK                  (** Socket *)
  with sexp

(** The informations returned by the {!UnixLabels.stat} calls. *)
type stats =
  Unix.LargeFile.stats =
    { st_dev : int;                       (** Device number *)
      st_ino : int;                       (** Inode number *)
      st_kind : file_kind;                (** Kind of the file *)
      st_perm : file_perm;                (** Access rights *)
      st_nlink : int;                     (** Number of links *)
      st_uid : int;                       (** User id of the owner *)
      st_gid : int;                       (** Group ID of the file's group *)
      st_rdev : int;                      (** Device minor number *)
      st_size : int64;                    (** Size in bytes *)
      
      st_atime : float;                   (** Last access time *)
      st_mtime : float;                   (** Last modification time *)
      st_ctime : float                    (** Last status change time *)
    } with sexp

(** Return the information for the named file. *)
val stat : string -> stats

(** Same as {!UnixLabels.stat}, but in case the file is a symbolic link,
   return the information for the link itself. *)
val lstat : string -> stats

(** Return the information for the file associated with the given
   descriptor. *)
val fstat : file_descr -> stats

(* This sub-module provides the normal OCaml Unix functions that deal with file size
  using native ints.  These are here because, in general, you should be using 64bit
  file operations so that large files aren't an issue.  If you have a real need to
  use potentially 31bit file operations (and you should be dubious of such a need) you
  can open this module *)
module Native_file : sig
  (** The informations returned by the {!UnixLabels.stat} calls. *)
  type stats = Unix.stats =
      { st_dev : int;                       (** Device number *)
        st_ino : int;                       (** Inode number *)
        st_kind : file_kind;                (** Kind of the file *)
        st_perm : file_perm;                (** Access rights *)
        st_nlink : int;                     (** Number of links *)
        st_uid : int;                       (** User id of the owner *)
        st_gid : int;                       (** Group ID of the file's group *)
        st_rdev : int;                      (** Device minor number *)
        st_size : int;                      (** Size in bytes *)
        st_atime : float;                   (** Last access time *)
        st_mtime : float;                   (** Last modification time *)
        st_ctime : float                    (** Last status change time *)
      }

  (** Return the information for the named file. *)
  val stat : string -> stats

  (** Same as {!UnixLabels.stat}, but in case the file is a symbolic link,
     return the information for the link itself. *)
  val lstat : string -> stats

  (** Return the information for the file associated with the given
     descriptor. *)
  val fstat : file_descr -> stats

  val lseek : file_descr -> int -> mode:seek_command -> int
  val truncate : string -> len:int -> unit
  val ftruncate : file_descr -> len:int -> unit
end

(** {6 Locking} *)



(** Commands for {!lockf}. *)
#undef F_ULOCK
#undef F_LOCK
#undef F_TLOCK
#undef F_TEST
#undef F_RLOCK
#undef F_TRLOCK
type lock_command =
  Unix.lock_command =
      F_ULOCK  (** Unlock a region *)
    | F_LOCK   (** Lock a region for writing, and block if already locked *)
    | F_TLOCK  (** Lock a region for writing, or fail if already locked *)
    | F_TEST   (** Test a region for other process locks *)
    | F_RLOCK  (** Lock a region for reading, and block if already locked *)
    | F_TRLOCK (** Lock a region for reading, or fail if already locked *)
;;

(** [lockf fd cmd size] place a lock on a file_descr that prevents any other process from
 * calling lockf successfully on the same file.  Due to a limitation in the current
 * implementation the length will be converted to a native int, potentially throwing an
 * exception if it is too large. *)
val lockf : file_descr -> mode:lock_command -> len:Core_int64.t -> unit

module Flock_command : sig
  (* These must never be reordered, the underlying C call relies on this order *)
  type t =
    | LOCK_SH
    | LOCK_EX
    | LOCK_UN
end

(** [flock fd cmd] places or releases a lock on the fd as per the flock C call of the same
    name. *)
val flock : file_descr -> Flock_command.t -> bool

(** Return [true] if the given file descriptor refers to a terminal or
   console window, [false] otherwise. *)
val isatty : file_descr -> bool

(* {6 Seeking, truncating and statistics on large files} *)


(** {6 Operations on file names} *)


(** Removes the named file *)
val unlink : string -> unit

(** [rename old new] changes the name of a file from [old] to [new]. *)
val rename : src:string -> dst:string -> unit

(** [link ?force ~target ~link_name ()] creates a hard link named [link_name]
    to the file named [target].  If [force] is true, an existing entry in
    place of [link_name] will be unlinked.  This unlinking may raise a Unix
    error, e.g. if the entry is a directory.

    @param force default = [false]
*)
val link :
  ?force : bool -> target : string -> link_name : string -> unit -> unit


(** {6 File permissions and ownership} *)

(** Change the permissions of the named file. *)
val chmod : string -> perm:file_perm -> unit

(** Change the permissions of an opened file. *)
val fchmod : file_descr -> perm:file_perm -> unit

(** Change the owner uid and owner gid of the named file. *)
val chown : string -> uid:int -> gid:int -> unit

(** Change the owner uid and owner gid of an opened file. *)
val fchown : file_descr -> uid:int -> gid:int -> unit

(** Set the process creation mask, and return the previous mask. *)
val umask : int -> int


(** Flags for the {!UnixLabels.access} call. *)
#undef R_OK
#undef W_OK
#undef X_OK
#undef F_OK
type access_permission =
  Unix.access_permission =
      R_OK                 (** Read permission *)
    | W_OK                 (** Write permission *)
    | X_OK                 (** Execution permission *)
    | F_OK                 (** File exists *)

(** Check that the process has the given permissions over the named
   file. Raise [Unix_error] otherwise. *)
val access : string -> perm:access_permission list -> unit



(** {6 Operations on file descriptors} *)


(** Return a new file descriptor referencing the same file as
   the given descriptor. *)
val dup : file_descr -> file_descr

(** [dup2 fd1 fd2] duplicates [fd1] to [fd2], closing [fd2] if already
   opened. *)
val dup2 : src:file_descr -> dst:file_descr -> unit

(** Set the ``non-blocking'' flag on the given descriptor.
   When the non-blocking flag is set, reading on a descriptor
   on which there is temporarily no data available raises the
   [EAGAIN] or [EWOULDBLOCK] error instead of blocking;
   writing on a descriptor on which there is temporarily no room
   for writing also raises [EAGAIN] or [EWOULDBLOCK]. *)
val set_nonblock : file_descr -> unit

(** Clear the ``non-blocking'' flag on the given descriptor.
   See {!UnixLabels.set_nonblock}.*)
val clear_nonblock : file_descr -> unit

(** Set the ``close-on-exec'' flag on the given descriptor.
   A descriptor with the close-on-exec flag is automatically
   closed when the current process starts another program with
   one of the [exec] functions. *)
val set_close_on_exec : file_descr -> unit

(** Clear the ``close-on-exec'' flag on the given descriptor.
   See {!UnixLabels.set_close_on_exec}.*)
val clear_close_on_exec : file_descr -> unit



(** {6 Directories} *)


(** Create a directory.  The permissions of the created directory are
    (perm & ~umask & 0777)
*)
val mkdir : string -> perm:file_perm -> unit

(** Remove an empty directory. *)
val rmdir : string -> unit

(** Change the process working directory. *)
val chdir : string -> unit

(** Return the name of the current working directory. *)
val getcwd : unit -> string

(** Change the process root directory. *)
val chroot : string -> unit

(** The type of descriptors over opened directories. *)
type dir_handle = Unix.dir_handle

(** Open a descriptor on a directory *)
val opendir : string -> dir_handle

(** Return the next entry in a directory.
   @raise End_of_file when the end of the directory has been reached. *)
val readdir : dir_handle -> string

(** Reposition the descriptor to the beginning of the directory *)
val rewinddir : dir_handle -> unit

(** Close a directory descriptor. *)
val closedir : dir_handle -> unit



(** {6 Pipes and redirections} *)


(** Create a pipe. The first component of the result is opened
   for reading, that's the exit to the pipe. The second component is
   opened for writing, that's the entrance to the pipe. *)
val pipe : unit -> file_descr * file_descr

(** Create a named pipe with the given permissions. *)
val mkfifo : string -> perm:file_perm -> unit


(** {6 High-level process and redirection management} *)

module Process_info : sig
  type t = {
    pid: int;
    stdin: file_descr;
    stdout: file_descr;
    stderr: file_descr
  }
end

(** Low-level process *)

(** [create_process ~prog ~args] forks a new process that
    executes the program [prog] with arguments [args].  The function
    returns the pid of the process along with file descriptors attached to
    stdin, stdout, and stderr of the new process.  The executable file
    [prog] is searched for in the path.  The new process has the same
    environment as the current process.  Unlike in [execve] the program
    name is automatically passed as the first argument. *)
val create_process :
  prog : string -> args : string list -> Process_info.t

(** [create_process_env ~prog ~args ~env] as create process, but takes an
 * additional parameter that extends, or replaces the current environment.
 *)
val create_process_env :
  prog : string
  -> args : string list
  
  -> env : [ `Replace of (string * string) list
           | `Extend of (string * string) list
           ]
  -> Process_info.t

(** High-level pipe and process management. These functions
   (with {!UnixLabels.open_process_out} and {!UnixLabels.open_process})
   run the given command in parallel with the program,
   and return channels connected to the standard input and/or
   the standard output of the command. The command is interpreted
   by the shell [/bin/sh] (cf. [system]). Warning: writes on channels
   are buffered, hence be careful to call {!Pervasives.flush} at the right times
   to ensure correct synchronization. *)
val open_process_in : string -> in_channel

(** See {!UnixLabels.open_process_in}. *)
val open_process_out : string -> out_channel

(** See {!UnixLabels.open_process_in}. *)
val open_process : string -> in_channel * out_channel

(** Similar to {!UnixLabels.open_process}, but the second argument specifies
   the environment passed to the command.  The result is a triple
   of channels connected to the standard output, standard input,
   and standard error of the command. *)
module Process_channels : sig
  type t = {
    stdin : out_channel;
    stdout : in_channel;
    stderr : in_channel;
  }
end

val open_process_full : string -> env:string array -> Process_channels.t

(* close_process_* raises Unix_error if, for example, the file descriptor has already been
   closed. *)

(** Close channels opened by {!UnixLabels.open_process_in},
    wait for the associated command to terminate,
    and return its termination status.
*)
val close_process_in : in_channel -> Process_status.t

(** Close channels opened by {!UnixLabels.open_process_out},
    wait for the associated command to terminate,
    and return its termination status.
*)
val close_process_out : out_channel -> Process_status.t

(** Close channels opened by {!UnixLabels.open_process},
   wait for the associated command to terminate,
   and return its termination status. *)
val close_process : in_channel * out_channel -> Process_status.t

(** Close channels opened by {!UnixLabels.open_process_full},
   wait for the associated command to terminate,
   and return its termination status. *)
val close_process_full : Process_channels.t -> Process_status.t

(** {6 Symbolic links} *)

(** [symlink source dest] creates the file [dest] as a symbolic link
   to the file [source]. *)
val symlink : src:string -> dst:string -> unit

(** Read the contents of a link. *)
val readlink : string -> string

(** {6 Polling} *)

(** Wait until some input/output operations become possible on
   some channels. The three list arguments are, respectively, a set
   of descriptors to check for reading (first argument), for writing
   (second argument), or for exceptional conditions (third argument).
   The fourth argument is the maximal timeout, in seconds; a
   negative fourth argument means no timeout (unbounded wait).
   The result is composed of three sets of descriptors: those ready
   for reading (first component), ready for writing (second component),
   and over which an exceptional condition is pending (third
   component). *)
module Select_fds : sig
  type t = {
    read : file_descr list;
    write : file_descr list;
    except : file_descr list;
  }

  val empty : t
end

(** Setting restart to true means that we want select to restart automatically
    on EINTR (instead of propagating the exception)... *)
val select:
  ?restart : bool
  -> read:file_descr list
  -> write:file_descr list
  -> except:file_descr list
  -> timeout:float
  -> unit
  -> Select_fds.t

(** Wait until a non-ignored, non-blocked signal is delivered. *)
val pause : unit -> unit

(** {6 Time functions} *)

(** The execution times (CPU times) of a process. *)
type process_times =
  Unix.process_times =
    { tms_utime : float;          (** User time for the process *)
      tms_stime : float;          (** System time for the process *)
      tms_cutime : float;         (** User time for the children processes *)
      tms_cstime : float;         (** System time for the children processes *)
    }

(** The type representing wallclock time and calendar date. *)
type tm =
  Unix.tm =
    { tm_sec : int;                       (** Seconds 0..59 *)
      tm_min : int;                       (** Minutes 0..59 *)
      tm_hour : int;                      (** Hours 0..23 *)
      tm_mday : int;                      (** Day of month 1..31 *)
      tm_mon : int;                       (** Month of year 0..11 *)
      tm_year : int;                      (** Year - 1900 *)
      tm_wday : int;                      (** Day of week (Sunday is 0) *)
      tm_yday : int;                      (** Day of year 0..365 *)
      tm_isdst : bool;                    (** Daylight time savings in effect *)
    }

(** Return the current time since 00:00:00 GMT, Jan. 1, 1970,
   in seconds. *)
val time : unit -> float


(** Same as {!UnixLabels.time}, but with resolution better than 1 second. *)
val gettimeofday : unit -> float

(** Convert a time in seconds, as returned by {!UnixLabels.time}, into a date and
   a time. Assumes UTC. *)
val gmtime : float -> tm

(** Convert a UTC time in a tm record to a time in seconds *)
val timegm : tm -> float

(** Convert a time in seconds, as returned by {!UnixLabels.time}, into a date and
   a time. Assumes the local time zone. *)
val localtime : float -> tm

(** Convert a date and time, specified by the [tm] argument, into
   a time in seconds, as returned by {!UnixLabels.time}. Also return a normalized
   copy of the given [tm] record, with the [tm_wday], [tm_yday],
   and [tm_isdst] fields recomputed from the other fields.
   The [tm] argument is interpreted in the local time zone. *)
val mktime : tm -> float * tm

(** Convert a date and time, specified by the [tm] argument, into a formatted string.
    See 'man strftime' for format options. *)
val strftime : tm -> string -> string

(** Schedule a [SIGALRM] signal after the given number of seconds. *)
val alarm : int -> int

(** Stop execution for the given number of seconds. *)
val sleep : int -> unit

(** Return the execution times of the process. *)
val times : unit -> process_times

(** Set the last access time (second arg) and last modification time
   (third arg) for a file. Times are expressed in seconds from
   00:00:00 GMT, Jan. 1, 1970. *)
val utimes : string -> access:float -> modif:float -> unit

(** The three kinds of interval timers. *)
type interval_timer =
  Unix.interval_timer =
      ITIMER_REAL
      (** decrements in real time, and sends the signal [SIGALRM] when expired.*)
    | ITIMER_VIRTUAL
      (**  decrements in process virtual time, and sends [SIGVTALRM] when expired. *)
    | ITIMER_PROF
      (** (for profiling) decrements both when the process
         is running and when the system is running on behalf of the
         process; it sends [SIGPROF] when expired. *)

(** The type describing the status of an interval timer *)
type interval_timer_status =
  Unix.interval_timer_status =
    { it_interval : float;          (** Period *)
      it_value : float;             (** Current value of the timer *)
    }

(** Return the current status of the given interval timer. *)
val getitimer : interval_timer -> interval_timer_status

(** [setitimer t s] sets the interval timer [t] and returns
   its previous status. The [s] argument is interpreted as follows:
   [s.it_value], if nonzero, is the time to the next timer expiration;
   [s.it_interval], if nonzero, specifies a value to
   be used in reloading it_value when the timer expires.
   Setting [s.it_value] to zero disable the timer.
   Setting [s.it_interval] to zero causes the timer to be disabled
   after its next expiration. *)
val setitimer :
  interval_timer -> interval_timer_status -> interval_timer_status


(** {6 User id, group id}
    It's highly recommended to read the straight unix docs on these functions for more
    color. You can get that info from man pages or
    http://www.opengroup.org/onlinepubs/000095399/functions/setuid.html
    *)


(** Return the user id of the user executing the process. *)
val getuid : unit -> int

(** Return the effective user id under which the process runs. *)
val geteuid : unit -> int

(** Sets the real user id and effective user id for the process. Only use this when
    superuser. To setuid as an ordinary user, see Core_extended.Unix.seteuid. *)
val setuid : int -> unit

(** Return the group id of the user executing the process. *)
val getgid : unit -> int

(** Return the effective group id under which the process runs. *)
val getegid : unit -> int

(** Set the real group id and effective group id for the process. *)
val setgid : int -> unit

(** Return the list of groups to which the user executing the process
   belongs. *)
val getgroups : unit -> int array

(** Structure of entries in the [passwd] database *)
module Passwd : sig
  type t =
    { name : string;
      passwd : string;
      uid : int;
      gid : int;
      gecos : string;
      dir : string;
      shell : string;
    }

  val getbyname : string -> t option
  val getbyname_exn : string -> t

  val getbyuid : int -> t option
  val getbyuid_exn : int -> t

  val setpwent : unit -> unit
  val endpwent : unit -> unit

  val getpwent : unit -> t option
  val getpwent_exn : unit -> t
end

(** Structure of entries in the [groups] database. *)
module Group : sig
  type t =
    { name : string;
      passwd : string;
      gid : int;
      mem : string array;
    }
  with sexp_of

  val getbyname : string -> t option
  val getbyname_exn : string -> t
  val getbygid : int -> t option
  val getbygid_exn : int -> t
end

(** Return the login name of the user executing the process. *)
val getlogin : unit -> string

(** {6 Internet addresses} *)


(** The abstract type of Internet addresses. *)
type inet_addr = Unix.inet_addr with sexp, bin_io

(** Conversion from the printable representation of an Internet
    address to its internal representation.  The argument string
    consists of 4 numbers separated by periods ([XXX.YYY.ZZZ.TTT])
    for IPv4 addresses, and up to 8 numbers separated by colons
    for IPv6 addresses.  Raise [Failure] when given a string that
    does not match these formats. *)
val inet_addr_of_string : string -> inet_addr

(** Return the printable representation of the given Internet address.
    See {!Unix.inet_addr_of_string} for a description of the
    printable representation. *)
val string_of_inet_addr : inet_addr -> string

(** A special IPv4 address, for use only with [bind], representing
   all the Internet addresses that the host machine possesses. *)
val inet_addr_any : inet_addr

(** A special IPv4 address representing the host machine ([127.0.0.1]). *)
val inet_addr_loopback : inet_addr

(** A special IPv6 address, for use only with [bind], representing
   all the Internet addresses that the host machine possesses. *)
val inet6_addr_any : inet_addr

(** A special IPv6 address representing the host machine ([::1]). *)
val inet6_addr_loopback : inet_addr


(** {6 Sockets} *)


(** The type of socket domains. *)
#undef PF_UNIX
#undef PF_INET
#undef PF_INET6
type socket_domain = Unix.socket_domain =
  | PF_UNIX                             (** Unix domain *)
  | PF_INET                             (** Internet domain *)
  | PF_INET6                            (* Internet domain (IPv6) *)
with sexp, bin_io

#undef SOCK_STREAM
#undef SOCK_DGRAM
#undef SOCK_RAW
#undef SOCK_SEQPACKET
(** The type of socket kinds, specifying the semantics of
   communications. *)
type socket_type = Unix.socket_type =
  | SOCK_STREAM                         (** Stream socket *)
  | SOCK_DGRAM                          (** Datagram socket *)
  | SOCK_RAW                            (** Raw socket *)
  | SOCK_SEQPACKET                      (* Sequenced packets socket *)
with sexp, bin_io

(** The type of socket addresses. [ADDR_UNIX name] is a socket
   address in the Unix domain; [name] is a file name in the file
   system. [ADDR_INET(addr,port)] is a socket address in the Internet
   domain; [addr] is the Internet address of the machine, and
   [port] is the port number. *)
type sockaddr = Unix.sockaddr =
  | ADDR_UNIX of string
  | ADDR_INET of inet_addr * int
with sexp, bin_io

(** Return the socket domain adequate for the given socket address. *)
val domain_of_sockaddr: sockaddr -> socket_domain

(** Create a new socket in the given domain, and with the
   given kind. The third argument is the protocol type; 0 selects
   the default protocol for that kind of sockets. *)
val socket :
  domain:socket_domain -> kind:socket_type -> protocol:int -> file_descr

(** Create a pair of unnamed sockets, connected together. *)
val socketpair :
  domain:socket_domain -> kind:socket_type -> protocol:int ->
    file_descr * file_descr

(** Accept connections on the given socket. The returned descriptor
   is a socket connected to the client; the returned address is
   the address of the connecting client. *)
val accept : file_descr -> file_descr * sockaddr

(** Bind a socket to an address. *)
val bind : file_descr -> addr:sockaddr -> unit

(** Connect a socket to an address. *)
val connect : file_descr -> addr:sockaddr -> unit

(** Set up a socket for receiving connection requests. The integer
   argument is the maximal number of pending requests. *)
val listen : file_descr -> max:int -> unit

(** The type of commands for [shutdown]. *)
type shutdown_command =
  Unix.shutdown_command =
      SHUTDOWN_RECEIVE                    (** Close for receiving *)
    | SHUTDOWN_SEND                       (** Close for sending *)
    | SHUTDOWN_ALL                        (** Close both *)

(** Shutdown a socket connection. [SHUTDOWN_SEND] as second argument
   causes reads on the other end of the connection to return
   an end-of-file condition.
   [SHUTDOWN_RECEIVE] causes writes on the other end of the connection
   to return a closed pipe condition ([SIGPIPE] signal). *)
val shutdown : file_descr -> mode:shutdown_command -> unit

(** Return the address of the given socket. *)
val getsockname : file_descr -> sockaddr

(** Return the address of the host connected to the given socket. *)
val getpeername : file_descr -> sockaddr

(** The flags for {!UnixLabels.recv},  {!UnixLabels.recvfrom},
   {!UnixLabels.send} and {!UnixLabels.sendto}. *)
#undef MSG_OOB
#undef MSG_DONTROUTE
#undef MSG_PEEK
type msg_flag = Unix.msg_flag =
    MSG_OOB
  | MSG_DONTROUTE
  | MSG_PEEK

(** Receive data from an unconnected socket. *)
val recv :
  file_descr -> buf:string -> pos:int -> len:int -> mode:msg_flag list -> int

(** Receive data from an unconnected socket. *)
val recvfrom :
  file_descr -> buf:string -> pos:int -> len:int -> mode:msg_flag list ->
    int * sockaddr

(** Send data over an unconnected socket. *)
val send :
  file_descr -> buf:string -> pos:int -> len:int -> mode:msg_flag list -> int

(** Send data over an unconnected socket. *)
val sendto :
  file_descr -> buf:string -> pos:int -> len:int -> mode:msg_flag list ->
    addr:sockaddr -> int


(** {6 Socket options} *)


(** The socket options that can be consulted with {!UnixLabels.getsockopt}
   and modified with {!UnixLabels.setsockopt}.  These options have a boolean
   ([true]/[false]) value. *)
#undef SO_DEBUG
#undef SO_BROADCAST
#undef SO_REUSEADDR
#undef SO_KEEPALIVE
#undef SO_DONTROUTE
#undef SO_OOBINLINE
#undef SO_ACCEPTCONN
type socket_bool_option =
    SO_DEBUG               (** Record debugging information *)
  | SO_BROADCAST           (** Permit sending of broadcast messages *)
  | SO_REUSEADDR           (** Allow reuse of local addresses for bind *)
  | SO_KEEPALIVE           (** Keep connection active *)
  | SO_DONTROUTE           (** Bypass the standard routing algorithms *)
  | SO_OOBINLINE           (** Leave out-of-band data in line *)
  | SO_ACCEPTCONN          (* Report whether socket listening is enabled *)
  | TCP_NODELAY            (** Control the Nagle algorithm for TCP sockets *)
  | IPV6_ONLY              (** Forbid binding an IPv6 socket to an IPv4 address *)

(** The socket options that can be consulted with {!UnixLabels.getsockopt_int}
   and modified with {!UnixLabels.setsockopt_int}.  These options have an
   integer value. *)
#undef SO_SNDBUF
#undef SO_RCVBUF
#undef SO_ERROR
#undef SO_TYPE
#undef SO_RCVLOWAT
#undef SO_SNDLOWAT
type socket_int_option =
    SO_SNDBUF              (** Size of send buffer *)
  | SO_RCVBUF              (** Size of received buffer *)
  | SO_ERROR               (** Report the error status and clear it *)
  | SO_TYPE                (** Report the socket type *)
  | SO_RCVLOWAT            (** Minimum number of bytes to process for input operations *)
  | SO_SNDLOWAT            (* Minimum number of bytes to process for output operations *)

(** The socket options that can be consulted with {!UnixLabels.getsockopt_optint}
   and modified with {!UnixLabels.setsockopt_optint}.  These options have a
   value of type [int option], with [None] meaning ``disabled''. *)
#undef SO_LINGER
type socket_optint_option =
    SO_LINGER              (** Whether to linger on closed connections
                              that have data present, and for how long
                              (in seconds) *)

(** The socket options that can be consulted with {!UnixLabels.getsockopt_float}
   and modified with {!UnixLabels.setsockopt_float}.  These options have a
   floating-point value representing a time in seconds.
   The value 0 means infinite timeout. *)
#undef SO_RCVTIMEO
#undef SO_SNDTIMEO
type socket_float_option =
    SO_RCVTIMEO            (** Timeout for input operations *)
  | SO_SNDTIMEO            (** Timeout for output operations *)


(** Return the current status of a boolean-valued option
   in the given socket. *)
val getsockopt : file_descr -> socket_bool_option -> bool

(** Set or clear a boolean-valued option in the given socket. *)
val setsockopt : file_descr -> socket_bool_option -> bool -> unit

(** Same as {!UnixLabels.getsockopt} for an integer-valued socket option. *)
val getsockopt_int : file_descr -> socket_int_option -> int

(** Same as {!UnixLabels.setsockopt} for an integer-valued socket option. *)
val setsockopt_int : file_descr -> socket_int_option -> int -> unit

(** Same as {!UnixLabels.getsockopt} for a socket option whose value is an [int option]. *)
val getsockopt_optint : file_descr -> socket_optint_option -> int option

(** Same as {!UnixLabels.setsockopt} for a socket option whose value is an [int option]. *)
val setsockopt_optint : file_descr -> socket_optint_option -> int option -> unit

(** Same as {!UnixLabels.getsockopt} for a socket option whose value is a floating-point
    number. *)
val getsockopt_float : file_descr -> socket_float_option -> float

(** Same as {!UnixLabels.setsockopt} for a socket option whose value is a floating-point
    number. *)
val setsockopt_float :
  file_descr -> socket_float_option -> float -> unit


(** {6 High-level network connection functions} *)


(** Connect to a server at the given address.
   Return a pair of buffered channels connected to the server.
   Remember to call {!Pervasives.flush} on the output channel at the right times
   to ensure correct synchronization. *)
val open_connection : sockaddr -> in_channel * out_channel

(** ``Shut down'' a connection established with {!UnixLabels.open_connection};
   that is, transmit an end-of-file condition to the server reading
   on the other side of the connection. *)
val shutdown_connection : in_channel -> unit

(** Establish a server on the given address.
   The function given as first argument is called for each connection
   with two buffered channels connected to the client. A new process
   is created for each connection. The function {!UnixLabels.establish_server}
   never returns normally. *)
val establish_server :
  (in_channel -> out_channel -> unit) -> addr:sockaddr -> unit


(** {6 Host and protocol databases} *)

(** Return the name of the local host. *)
val gethostname : unit -> string

module Host : sig
  (** Structure of entries in the [hosts] database. *)
  type t =
    { name : string;
      aliases : string array;
      addrtype : socket_domain;
      addrs : inet_addr array;
    }

  (** Find an entry in [hosts] with the given name. *)
  val getbyname : string -> t option
  val getbyname_exn : string -> t

  (** Find an entry in [hosts] with the given address. *)
  val getbyaddr : inet_addr -> t option
  val getbyaddr_exn : inet_addr -> t
end

module Protocol : sig
  (** Structure of entries in the [protocols] database. *)
  type t =
    { name : string;
      aliases : string array;
      proto : int;
    }

  (** Find an entry in [protocols] with the given name. *)
  val getbyname : string -> t option
  val getbyname_exn : string -> t

  (** Find an entry in [protocols] with the given protocol number. *)
  val getbynumber : int -> t option
  val getbynumber_exn : int -> t
end

module Service : sig
  (** Structure of entries in the [services] database. *)
  type t =
    { name : string;
      aliases : string array;
      port : int;
      proto : string;
    }

  (** Find an entry in [services] with the given name. *)
  val getbyname : string -> protocol:string -> t option
  val getbyname_exn : string -> protocol:string -> t

  (** Find an entry in [services] with the given service number. *)
  val getbyport : int -> protocol:string -> t option
  val getbyport_exn : int -> protocol:string -> t
end

(** Address information returned by {!Unix.getaddrinfo}. *)
type addr_info =
  { ai_family : socket_domain;          (** Socket domain *)
    ai_socktype : socket_type;          (** Socket type *)
    ai_protocol : int;                  (** Socket protocol number *)
    ai_addr : sockaddr;                 (** Address *)
    ai_canonname : string               (* Canonical host name  *)
  }

(** Options to {!Unix.getaddrinfo}. *)
type getaddrinfo_option =
    AI_FAMILY of socket_domain          (** Impose the given socket domain *)
  | AI_SOCKTYPE of socket_type          (** Impose the given socket type *)
  | AI_PROTOCOL of int                  (** Impose the given protocol  *)
  | AI_NUMERICHOST                      (** Do not call name resolver,
                                            expect numeric IP address *)
  | AI_CANONNAME                        (** Fill the [ai_canonname] field
                                            of the result *)
  | AI_PASSIVE                          (* Set address to ``any'' address
                                            for use with {!Unix.bind} *)

(** [getaddrinfo host service opts] returns a list of {!Unix.addr_info}
    records describing socket parameters and addresses suitable for
    communicating with the given host and service.  The empty list is
    returned if the host or service names are unknown, or the constraints
    expressed in [opts] cannot be satisfied.

    [host] is either a host name or the string representation of an IP
    address.  [host] can be given as the empty string; in this case,
    the ``any'' address or the ``loopback'' address are used,
    depending whether [opts] contains [AI_PASSIVE].
    [service] is either a service name or the string representation of
    a port number.  [service] can be given as the empty string;
    in this case, the port field of the returned addresses is set to 0.
    [opts] is a possibly empty list of options that allows the caller
    to force a particular socket domain (e.g. IPv6 only, or IPv4 only)
    or a particular socket type (e.g. TCP only or UDP only). *)
val getaddrinfo:
  string -> string -> getaddrinfo_option list -> addr_info list

(** Host and service information returned by {!Unix.getnameinfo}. *)
type name_info =
  { ni_hostname : string;               (** Name or IP address of host *)
    ni_service : string }               (** Name of service or port number *)

(** Options to {!Unix.getnameinfo}. *)
type getnameinfo_option =
    NI_NOFQDN            (** Do not qualify local host names *)
  | NI_NUMERICHOST       (** Always return host as IP address *)
  | NI_NAMEREQD          (** Fail if host name cannot be determined *)
  | NI_NUMERICSERV       (** Always return service as port number *)
  | NI_DGRAM             (* Consider the service as UDP-based
                             instead of the default TCP *)

(** [getnameinfo addr opts] returns the host name and service name
    corresponding to the socket address [addr].  [opts] is a possibly
    empty list of options that governs how these names are obtained.
    Raise [Not_found] if an error occurs. *)
val getnameinfo : sockaddr -> getnameinfo_option list -> name_info


(** {2 Getting terminal size} *)

(** [get_terminal_size ()] @return [(rows, cols)], the number of rows and
    columns of the terminal. *)
val get_terminal_size : unit -> int * int

(** {6 Terminal interface} *)

(** The following functions implement the POSIX standard terminal
   interface. They provide control over asynchronous communication ports
   and pseudo-terminals. Refer to the [termios] man page for a complete
   description. *)

module Terminal_io : sig
  include Terminal_io_intf.Types

  (** Return the status of the terminal referred to by the given
      file descriptor. *)
  val tcgetattr : file_descr -> t

  
  (** Set the status of the terminal referred to by the given
     file descriptor. The second argument indicates when the
     status change takes place: immediately ([TCSANOW]),
     when all pending output has been transmitted ([TCSADRAIN]),
     or after flushing all input that has been received but not
     read ([TCSAFLUSH]). [TCSADRAIN] is recommended when changing
     the output parameters; [TCSAFLUSH], when changing the input
     parameters. *)
  val tcsetattr : file_descr -> mode:setattr_when -> t -> unit

  (** Send a break condition on the given file descriptor.
     The second argument is the duration of the break, in 0.1s units;
     0 means standard duration (0.25s). *)
  val tcsendbreak : file_descr -> duration:int -> unit

  (** Waits until all output written on the given file descriptor
     has been transmitted. *)
  val tcdrain : file_descr -> unit

  type flush_queue =
      Unix.flush_queue =
      TCIFLUSH
    | TCOFLUSH
    | TCIOFLUSH

  (** Discard data written on the given file descriptor but not yet
     transmitted, or data received but not yet read, depending on the
     second argument: [TCIFLUSH] flushes data received but not read,
     [TCOFLUSH] flushes data written but not transmitted, and
     [TCIOFLUSH] flushes both. *)
  val tcflush : file_descr -> mode:flush_queue -> unit

  type flow_action =
      Unix.flow_action =
      TCOOFF
    | TCOON
    | TCIOFF
    | TCION

  (** Suspend or restart reception or transmission of data on
     the given file descriptor, depending on the second argument:
     [TCOOFF] suspends output, [TCOON] restarts output,
     [TCIOFF] transmits a STOP character to suspend input,
     and [TCION] transmits a START character to restart input. *)
  val tcflow : file_descr -> mode:flow_action -> unit

  (** Put the calling process in a new session and detach it from
     its controlling terminal. *)
  val setsid : unit -> int
end


(** {6 Jane Street extensions} *)

(** Get inet_addr of a hostname or IP *)
val get_inet_addr : string -> inet_addr

(** Get a sockaddr from a hostname or IP, and a port *)
val get_sockaddr : string -> int -> sockaddr

(** Set a timeout for a socket associated with an [in_channel] *)
val set_in_channel_timeout : in_channel -> float -> unit

(** Set a timeout for a socket associated with an [out_channel] *)
val set_out_channel_timeout : out_channel -> float -> unit


(** {2 Utility functions} *)

external exit_immediately : int -> _ = "caml_sys_exit"
(** [exit_immediately exit_code] immediately calls the [exit] system call
    with the given exit code without performing any other actions
    (unlike Pervasives.exit).  Does not return. *)


(** {2 Filesystem functions} *)

val mknod :
  ?file_kind : file_kind ->
  ?perm : int ->
  ?major : int ->
  ?minor : int ->
  string -> unit
(** [mknod ?file_kind ?perm ?major ?minor path] creates a filesystem
    entry.  Note that only FIFO-entries are guaranteed to be supported
    across all platforms as required by the POSIX-standard.  On Linux
    directories and symbolic links cannot be created with this function.
    Use {!Unix.mkdir} and {!Unix.symlink} instead there respectively.

    @raise Invalid_argument if an unsupported file kind is used.
    @raise Unix_error if the system call fails.

    @param file_kind default = [S_REG] (= regular file)
    @param perm default = [0o600] (= read/write for user only)
    @param major default = [0]
    @param minor default = [0]
*)


(** {2 I/O vectors} *)

(** I/O-vectors for scatter/gather-operations *)
module IOVec : sig
  open Bigarray

  (** Representation of I/O-vectors.
      NOTE: DO NOT CHANGE THE MEMORY LAYOUT OF THIS TYPE!!!
      All C-functions in our bindings that handle I/O-vectors depend on it.
  *)
  type 'buf t = private
    {
      buf : 'buf;  (** Buffer holding the I/O-vector *)
      pos : int;  (** Position of I/O-vector in buffer *)
      len : int;  (** Length of I/O-vector in buffer *)
    }

  type 'buf kind  (** Kind of I/O-vector buffers *)

  type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

  val string_kind : string kind
  val bigstring_kind : bigstring kind

  val empty : 'buf kind -> 'buf t
  (** [empty] the empty I/O-vector. *)

  val of_string : ?pos : int -> ?len : int -> string -> string t
  (** [of_string ?pos ?len str] @return an I/O-vector designated by
      position [pos] and length [len] in string [str].

      @raise Invalid_argument if designated substring out of bounds.

      @param pos default = 0
      @param len default = [String.length str - pos]
  *)

  val of_bigstring : ?pos : int -> ?len : int -> bigstring -> bigstring t
  (** [of_bigstring ?pos ?len bstr] @return an I/O-vector designated by
      position [pos] and length [len] in bigstring [bstr].

      @raise Invalid_argument if designated substring out of bounds.

      @param pos default = 0
      @param len default = [String.length str - pos]
  *)

  val drop : 'buf t -> int -> 'buf t
  (** [drop iovec n] drops [n] characters from [iovec].  @return resulting
      I/O-vector.

      @raise Failure if [n] is greater than length of [iovec].
  *)

  val max_iovecs : int
end


(** {2 I/O functions} *)

external int_of_file_descr : file_descr -> int = "%identity"
(** [int_of_file_descr fd] converts file descriptor [fd] to the internal
    integer value. *)

external file_descr_of_int : int -> file_descr = "%identity"
(** [file_descr_of_int n] converts an integer to a file descriptor. *)

external dirfd : dir_handle -> file_descr = "unix_dirfd"

(** Extract a file descriptor from a directory handle. *)

external sync : unit -> unit = "unix_sync"
(** Synchronize all filesystem buffers with disk. *)

external fsync : file_descr -> unit = "unix_fsync"
(** Synchronize the kernel buffers of a given file descriptor with disk. *)

#if defined(_POSIX_SYNCHRONIZED_IO) && _POSIX_SYNCHRONIZED_IO > 0
external fdatasync : file_descr -> unit = "unix_fdatasync"
(** Synchronize the kernel buffers of a given file descriptor with disk,
    but do not necessarily write file attributes. *)
#else
#warning "_POSIX_SYNCHRONIZED_IO undefined or <= 0, fdatasync unavailable"
#endif

external readdir_ino :
  dir_handle -> string * nativeint = "unix_readdir_ino_stub"
(** [readdir_ino dh] return the next entry in a directory (([(filename,
    inode)]).  @raise End_of_file when the end of the directory has been
    reached. *)

val read_assume_fd_is_nonblocking :
  file_descr -> ?pos : int -> ?len : int -> string -> int
(** [read_assume_fd_is_nonblocking fd ?pos ?len buf] calls the system call
    [read] ASSUMING THAT IT IS NOT GOING TO BLOCK.  Reads at most [len]
    bytes into buffer [buf] starting at position [pos].  @return the
    number of bytes actually read.

    @raise Invalid_argument if buffer range out of bounds.
    @raise Unix_error on Unix-errors.

    @param pos = 0
    @param len = [String.length buf - pos]
*)

val write_assume_fd_is_nonblocking :
  file_descr -> ?pos : int -> ?len : int -> string -> int
(** [write_assume_fd_is_nonblocking fd ?pos ?len buf] calls the system call
    [write] ASSUMING THAT IT IS NOT GOING TO BLOCK.  Writes at most [len]
    bytes from buffer [buf] starting at position [pos].  @return the
    number of bytes actually written.

    @raise Invalid_argument if buffer range out of bounds.
    @raise Unix_error on Unix-errors.

    @param pos = 0
    @param len = [String.length buf - pos]
*)

val writev_assume_fd_is_nonblocking :
  file_descr -> ?count : int -> string IOVec.t array -> int
(** [writev_assume_fd_is_nonblocking fd ?count iovecs] calls the system call
    [writev] ASSUMING THAT IT IS NOT GOING TO BLOCK using [count]
    I/O-vectors [iovecs].  @return the number of bytes actually written.

    @raise Invalid_argument if the designated ranges are invalid.
    @raise Unix_error on Unix-errors.
*)

val writev : file_descr -> ?count : int -> string IOVec.t array -> int
(** [writev fd ?count iovecs] like {!writev_assume_fd_is_nonblocking}, but does
    not require the descriptor to not block.  If you feel you have to
    use this function, you should probably have chosen I/O-vectors that
    build on bigstrings, because this function has to internally blit
    the I/O-vectors (ordinary OCaml strings) to intermediate buffers on
    the C-heap.

    @return the number of bytes actually written.

    @raise Invalid_argument if the designated ranges are invalid.
    @raise Unix_error on Unix-errors.
*)

external pselect :
  file_descr list -> file_descr list -> file_descr list -> float -> int list ->
  file_descr list * file_descr list * file_descr list = "unix_pselect_stub"
(** [pselect rfds wfds efds timeout sigmask] like {!Core_unix.select} but
    also allows one to wait for the arrival of signals. *)

(** {2 Resource limits} *)
module RLimit : sig
  type limit = Limit of int64 | Infinity with sexp

  type t = {
    cur : limit;  (* soft limit *)
    max : limit;  (* hard limit (ceiling for soft limit) *)
  } with sexp

  type resource = [
    | `Core_file_size
    | `Cpu_seconds
    | `Data_segment
    | `File_size
    | `Num_file_descriptors
    | `Stack
    | `Virtual_memory
  ] with sexp

  (* See man pages for "getrlimit" and "setrlimit" for details. *)
  val get : resource -> t
  val set : resource -> t -> unit
end

(** {2 Resource usage} -- For details, "man getrusage" *)
module Resource_usage : sig
  
  type who = SELF | CHILDREN

  type t

  val get : who -> t

  val utime : t -> float      (* user time used *)
  val stime : t -> float      (* system time used *)
  val maxrss : t -> int64     (* maximum resident set size *)
  val ixrss : t -> int64      (* integral shared memory size *)
  val idrss : t -> int64      (* integral unshared data size *)
  val isrss : t -> int64      (* integral unshared stack size *)
  val minflt : t -> int64     (* page reclaims *)
  val majflt : t -> int64     (* page faults *)
  val nswap : t -> int64      (* swaps *)
  val inblock : t -> int64    (* block input operations *)
  val oublock : t -> int64    (* block output operations *)
  val msgsnd : t -> int64     (* messages sent *)
  val msgrcv : t -> int64     (* messages received *)
  val nsignals : t -> int64   (* signals received *)
  val nvcsw : t -> int64      (* voluntary context switches *)
  val nivcsw : t -> int64     (* involuntary context switches *)

  (** [add ru1 ru2] adds two rusage structures (e.g. your resource usage
      and your children's). *)
  val add : t -> t -> t
end

(** {2 System configuration} *)

#undef ARG_MAX
#undef CHILD_MAX
#undef HOST_NAME_MAX
#undef LOGIN_NAME_MAX
#undef OPEN_MAX
#undef PAGESIZE
#undef RE_DUP_MAX
#undef STREAM_MAX
#undef SYMLOOP_MAX
#undef TTY_NAME_MAX
#undef TZNAME_MAX
#undef POSIX_VERSION
#undef PHYS_PAGES
#undef AVPHYS_PAGES
#undef IOV_MAX
type sysconf =
  | ARG_MAX
  | CHILD_MAX
  | HOST_NAME_MAX
  | LOGIN_NAME_MAX
  | OPEN_MAX
  | PAGESIZE
  | RE_DUP_MAX
  | STREAM_MAX
  | SYMLOOP_MAX
  | TTY_NAME_MAX
  | TZNAME_MAX
  | POSIX_VERSION
  | PHYS_PAGES
  | AVPHYS_PAGES
  | IOV_MAX

external sysconf : sysconf -> int64 = "unix_sysconf"

(** {2 Temporary file and directory creation} *)

(** [mkstemp prefix] creates and opens a unique temporary file with
    [prefix], automatically appending a suffix of six random characters
    to make the name unique.

    @raise Unix_error on errors.
*)
val mkstemp : string -> string * Unix.file_descr

(** [mkdtemp prefix] creates a temporary directory with [prefix],
    automatically appending a suffix of six random characters to make
    the name unique.

    @raise Unix_error on errors.
k*)
val mkdtemp : string -> string

(** {2 Signal handling} *)

(* Causes abnormal program termination unless the signal SIGABRT is
   caught and the signal handler does not return.  If the SIGABRT signal is
   blocked or ignored, the abort() function will still override it.
*)
external abort : unit -> _ = "unix_abort" "noalloc"

(** {2 User id, group id} *)

external initgroups : string -> int -> unit = "unix_initgroups"

(** {2 Globbing and shell expansion} *)

(* no system calls involved *)
val fnmatch :
  ?flags :
    [ `No_escape
    | `Pathname
    | `Period
    | `File_name
    | `Leading_dir
    | `Casefold ] list -> pat : string -> string -> bool

(* See man page for wordexp. *)
val wordexp :
  ?flags : [ `No_cmd | `Show_err | `Undef ] list -> string -> string array

(** {2 System information} *)

module Utsname : sig
  type t
  val sysname : t -> string
  val nodename : t -> string
  val release : t -> string
  val version : t -> string
  val machine : t -> string
end

(* See man page for uname. *)
val uname : unit -> Utsname.t

(** {2 Additional IP functionality} *)

(* [if_indextoname ifindex] If [ifindex] is an interface index, then
   the function returns the interface name.  Otherwise, it raises
   [Unix_error]. *)
external if_indextoname : int -> string = "unix_if_indextoname"

(** [mcast_join ?ifname sock addr] join a multicast group at [addr]
    with socket [sock], optionally using network interface [ifname].

    @param ifname default = any interface
*)
external mcast_join :
  ?ifname : string -> file_descr -> sockaddr -> unit = "unix_mcast_join"

(** [mcast_leave ?ifname sock addr] leaves a multicast group at [addr]
    with socket [sock], optionally using network interface [ifname].

    @param ifname default = any interface
*)
external mcast_leave :
  ?ifname : string -> file_descr -> sockaddr -> unit = "unix_mcast_leave"

module Scheduler : sig
  module Policy : sig
    type t = [ `Fifo | `Round_robin | `Other ] with sexp
  end

  (* See [man sched_setscheduler]. *)
  val set : pid : int -> policy : Policy.t -> priority : int -> unit
end

module Priority : sig
  external nice : int -> int = "unix_nice"
end

(** [get_num_open_fds ()] @return number of open file descriptors.  It walks
    through every possible file descriptor and calls fcntl() to see if it
    is valid, which may be slow.  This function can be useful for detecting
    descriptor leaks. *)
val get_num_open_fds : unit -> int

(* For keeping your memory in RAM, i.e. preventing it from being swapped out. *)
module Mman : sig
  module Mcl_flags : sig
    type t = Current | Future with sexp
  end

  (* Lock all pages in this processes virtual address space into physical memory. See [man
     mlockall] for more details. *)
  val mlockall : Mcl_flags.t list -> unit

  (* Unlock previously locked pages. See [man munlockall]. *)
  val munlockall : unit -> unit
end

(* vim: set filetype=ocaml : *)
