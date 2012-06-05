(*start-pp-include
#include <unistd.h>
end-pp-include*)



(* Core_unix wraps the standard unix functions with an exception handler that
   inserts an informative string in the third field of Unix_error.  The problem
   with the standard Unix_error that gets raised is that it doesn't include
   information about the arguments to the function that failed.
*)

open Std_internal

module Unix = Caml.UnixLabels

open Sexplib.Conv

let sprintf = Printf.sprintf

external sync : unit -> unit = "unix_sync"
external fsync : Unix.file_descr -> unit = "unix_fsync"
#if defined(_POSIX_SYNCHRONIZED_IO) && _POSIX_SYNCHRONIZED_IO > 0
external fdatasync : Unix.file_descr -> unit = "unix_fdatasync"
#else
#warning "_POSIX_SYNCHRONIZED_IO undefined or <= 0, fdatasync unavailable"
#endif

external dirfd : Unix.dir_handle -> Unix.file_descr = "unix_dirfd"

external readdir_ino :
  Unix.dir_handle -> string * nativeint = "unix_readdir_ino_stub"

external int_of_file_descr : Unix.file_descr -> int = "%identity"
external file_descr_of_int : int -> Unix.file_descr = "%identity"

external unsetenv : string -> unit = "unix_unsetenv"

external exit_immediately : int -> _ = "caml_sys_exit"

external unsafe_read_assume_fd_is_nonblocking :
  Unix.file_descr -> string -> pos : int -> len : int -> int
    = "unix_read_assume_fd_is_nonblocking_stub"

let check_string_args ~loc str ~pos ~len =
  if pos < 0 then invalid_arg (loc ^ ": pos < 0");
  if len < 0 then invalid_arg (loc ^ ": len < 0");
  let str_len = String.length str in
  if str_len < pos + len then
    invalid_arg (Printf.sprintf "Unix_ext.%s: length(str) < pos + len" loc)

let get_opt_pos ~loc = function
  | Some pos ->
    
    if pos < 0 then invalid_arg (Printf.sprintf "Unix_ext.%s: pos < 0" loc);
    pos
  | None -> 0

let get_opt_len str ~pos = function
  | Some len -> len
  | None -> String.length str - pos

let read_assume_fd_is_nonblocking fd ?pos ?len buf =
  let loc = "read_assume_fd_is_nonblocking" in
  let pos = get_opt_pos ~loc pos in
  let len = get_opt_len buf ~pos len in
  check_string_args ~loc buf ~pos ~len;
  unsafe_read_assume_fd_is_nonblocking fd buf ~pos ~len

external unsafe_write_assume_fd_is_nonblocking :
  Unix.file_descr -> string -> pos : int -> len : int -> int
    = "unix_write_assume_fd_is_nonblocking_stub"

let write_assume_fd_is_nonblocking fd ?pos ?len buf =
  let loc = "write_assume_fd_is_nonblocking" in
  let pos = get_opt_pos ~loc pos in
  let len = get_opt_len buf ~pos len in
  check_string_args ~loc buf ~pos ~len;
  unsafe_write_assume_fd_is_nonblocking fd buf ~pos ~len


(* Filesystem functions *)

external mknod :
  string -> Unix.file_kind -> int -> int -> int -> unit = "unix_mknod_stub"

(* unistd.h defines minor and major when used with glibc ...*)
#ifdef minor
# undef minor
# undef major
#endif
let mknod
    ?(file_kind = Unix.S_REG) ?(perm = 0o600) ?(major = 0) ?(minor = 0)
    pathname =
  mknod pathname file_kind perm major minor

(* Resource limits *)

module RLimit = struct
  type limit = Limit of int64 | Infinity with sexp
  type t = { cur : limit; max : limit } with sexp

  type resource = [
    | `Core_file_size
    | `Cpu_seconds
    | `Data_segment
    | `File_size
    | `Num_file_descriptors
    | `Stack
    | `Virtual_memory
  ] with sexp

  type resource_param =
    | Core_file_size
    | Cpu_seconds
    | Data_segment
    | File_size
    | Num_file_descriptors
    | Stack
    | Virtual_memory
  ;;

  let resource_param_of_resource = function
    | `Core_file_size -> Core_file_size
    | `Cpu_seconds -> Cpu_seconds
    | `Data_segment -> Data_segment
    | `File_size -> File_size
    | `Num_file_descriptors -> Num_file_descriptors
    | `Stack -> Stack
    | `Virtual_memory -> Virtual_memory
  ;;

  external get : resource_param -> t = "unix_getrlimit"
  external set : resource_param -> t -> unit = "unix_setrlimit"

  let get resource = get (resource_param_of_resource resource);;
  let set resource t = set (resource_param_of_resource resource) t;;
end


(* Resource usage *)

module Resource_usage = struct
  type who = SELF | CHILDREN

  type t = {
    utime : float;
    stime : float;
    maxrss : int64;
    ixrss : int64;
    idrss : int64;
    isrss : int64;
    minflt : int64;
    majflt : int64;
    nswap : int64;
    inblock : int64;
    oublock : int64;
    msgsnd : int64;
    msgrcv : int64;
    nsignals : int64;
    nvcsw : int64;
    nivcsw : int64;
  }

  external getrusage : who -> t = "unix_getrusage"

  let get who = getrusage who

  let utime t = t.utime
  let stime t = t.stime
  let maxrss t = t.maxrss
  let ixrss t = t.ixrss
  let idrss t = t.idrss
  let isrss t = t.isrss
  let minflt t = t.minflt
  let majflt t = t.majflt
  let nswap t = t.nswap
  let inblock t = t.inblock
  let oublock t = t.oublock
  let msgsnd t = t.msgsnd
  let msgrcv t = t.msgrcv
  let nsignals t = t.nsignals
  let nvcsw t = t.nvcsw
  let nivcsw t = t.nivcsw

  let add t1 t2 = {
    utime = t1.utime +. t2.utime;
    stime = t1.stime +. t2.stime;
    maxrss = Int64.(+) t1.maxrss t2.maxrss;
    ixrss = Int64.(+) t1.ixrss t2.ixrss;
    idrss = Int64.(+) t1.idrss t2.idrss;
    isrss = Int64.(+) t1.isrss t2.isrss;
    minflt = Int64.(+) t1.minflt t2.minflt;
    majflt = Int64.(+) t1.majflt t2.majflt;
    nswap = Int64.(+) t1.nswap t2.nswap;
    inblock = Int64.(+) t1.inblock t2.inblock;
    oublock = Int64.(+) t1.oublock t2.oublock;
    msgsnd = Int64.(+) t1.msgsnd t2.msgsnd;
    msgrcv = Int64.(+) t1.msgrcv t2.msgrcv;
    nsignals = Int64.(+) t1.nsignals t2.nsignals;
    nvcsw = Int64.(+) t1.nvcsw t2.nvcsw;
    nivcsw = Int64.(+) t1.nivcsw t2.nivcsw;
  }
end


(* System configuration *)

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


(* I/O vectors *)

module IOVec = struct
  open Bigarray

  (* NOTE: DO NOT CHANGE THE MEMORY LAYOUT OF THIS TYPE!!! *)
  type 'buf t =
    {
      buf : 'buf;
      pos : int;
      len : int;
    }

  type 'buf kind = 'buf

  type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

  let string_kind = ""
  let bigstring_kind = Array1.create Bigarray.char c_layout 0

  let empty kind =
    {
      buf = kind;
      pos = 0;
      len = 0;
    }

  let get_iovec loc ?pos ?len true_len buf =
    let pos =
      match pos with
      | None -> 0
      | Some pos ->
          if pos < 0 then invalid_arg (loc ^ ": pos < 0");
          pos
    in
    let len =
      match len with
      | None -> true_len
      | Some len ->
          if len < 0 then invalid_arg (loc ^ ": len < 0");
          len
    in
    if pos + len > true_len then invalid_arg (loc ^ ": pos + len > length buf");
    {
      buf = buf;
      pos = pos;
      len = len;
    }

  let of_string ?pos ?len str =
    let str_len = String.length str in
    get_iovec "IOVec.of_string" ?pos ?len str_len str

  let of_bigstring ?pos ?len bstr =
    let bstr_len = Array1.dim bstr in
    get_iovec "IOVec.of_bigstring" ?pos ?len bstr_len bstr

  let drop iovec n =
    if n > iovec.len then failwith "IOVec.drop: n > length iovec"
    else
      {
        buf = iovec.buf;
        pos = iovec.pos + n;
        len = iovec.len - n;
      }

  let max_iovecs =
    let n64 = sysconf IOV_MAX in
    if n64 > Int64.of_int Sys.max_array_length then Sys.max_array_length
    else Int64.to_int_exn n64
end

let get_iovec_count loc iovecs = function
  | None -> Array.length iovecs
  | Some count ->
      if count < 0 then invalid_arg (loc ^ ": count < 0");
      let n_iovecs = Array.length iovecs in
      if count > n_iovecs then invalid_arg (loc ^ ": count > n_iovecs");
      count

external unsafe_writev_assume_fd_is_nonblocking :
  Unix.file_descr -> string IOVec.t array -> int -> int
  = "unix_writev_assume_fd_is_nonblocking_stub"

let writev_assume_fd_is_nonblocking fd ?count iovecs =
  let count = get_iovec_count "writev_assume_fd_is_nonblocking" iovecs count in
  unsafe_writev_assume_fd_is_nonblocking fd iovecs count

external unsafe_writev :
  Unix.file_descr -> string IOVec.t array -> int -> int = "unix_writev_stub"

let writev fd ?count iovecs =
  let count = get_iovec_count "writev" iovecs count in
  unsafe_writev fd iovecs count

external pselect :
  Unix.file_descr list -> Unix.file_descr list -> Unix.file_descr list ->
  float -> int list ->
  Unix.file_descr list * Unix.file_descr list * Unix.file_descr list
  = "unix_pselect_stub"


(* Temporary file and directory creation *)


external mkstemp : string -> string * Unix.file_descr = "unix_mkstemp"
external mkdtemp : string -> string = "unix_mkdtemp"


(* Signal handling *)

external abort : unit -> 'a = "unix_abort" "noalloc"


(* User id, group id management *)

external initgroups : string -> int -> unit = "unix_initgroups"


(** Globbing and shell word expansion *)

module Fnmatch_flags = struct
  type flag = [
    | `No_escape
    | `Pathname
    | `Period
    | `File_name
    | `Leading_dir
    | `Casefold
  ]

  let flag_to_internal = function
    | `No_escape -> 0
    | `Pathname -> 1
    | `Period -> 2
    | `File_name -> 3
    | `Leading_dir -> 4
    | `Casefold -> 5

  type t = int32

  external internal_make : int array -> t = "unix_fnmatch_make_flags"

  let make = function
    | None | Some [] -> Int32.zero
    | Some flags ->
        internal_make (Array.map ~f:flag_to_internal (Array.of_list flags))
end

external fnmatch :
  Fnmatch_flags.t -> pat : string -> string -> bool = "unix_fnmatch"

let fnmatch ?flags ~pat fname = fnmatch (Fnmatch_flags.make flags) ~pat fname

module Wordexp_flags = struct
  type flag = [ `No_cmd | `Show_err | `Undef ]

  let flag_to_internal = function
    | `No_cmd -> 0
    | `Show_err -> 1
    | `Undef -> 2

  type t = int32

  external internal_make : int array -> t = "unix_wordexp_make_flags"

  let make = function
    | None | Some [] -> Int32.zero
    | Some flags ->
        internal_make (Array.map ~f:flag_to_internal (Array.of_list flags))
end

external wordexp : Wordexp_flags.t -> string -> string array = "unix_wordexp"

let wordexp ?flags str = wordexp (Wordexp_flags.make flags) str


(* System information *)

module Utsname = struct
  type t = {
    sysname: string;
    nodename: string;
    release: string;
    version: string;
    machine: string;
  }

  let sysname t = t.sysname
  let nodename t = t.nodename
  let release t = t.release
  let version t = t.version
  let machine t = t.machine
end

external uname : unit -> Utsname.t = "unix_uname"


(* Additional IP functionality *)

external if_indextoname : int -> string = "unix_if_indextoname"

external mcast_join :
  ?ifname : string -> Unix.file_descr -> Unix.sockaddr -> unit
  = "unix_mcast_join"

external mcast_leave :
  ?ifname : string -> Unix.file_descr -> Unix.sockaddr -> unit
  = "unix_mcast_leave"

module Scheduler = struct
  module Policy = struct
    type t = [ `Fifo | `Round_robin | `Other ] with sexp

    module Ordered = struct
      type t = Fifo | Round_robin | Other
      let create = function
        | `Fifo -> Fifo
        | `Round_robin -> Round_robin
        | `Other -> Other
    end
  end

  external set :
    pid : int -> policy : Policy.Ordered.t -> priority : int -> unit
    = "unix_sched_setscheduler"

  let set ~pid ~policy ~priority =
    set ~pid ~policy:(Policy.Ordered.create policy) ~priority
end

module Priority = struct
  external nice : int -> int = "unix_nice"
end

external get_num_open_fds : unit -> int = "unix_get_num_open_fds"

module Mman = struct
  

  module Mcl_flags = struct
    type t = Current | Future with sexp ;;
    
    let int_of_t = function
      (* these bits were copied from bits/mman.h *)
      | Current -> 1
      | Future -> 2
    ;;
    let int_of_t_list lst =
      List.map lst ~f:int_of_t
      |! List.fold ~init:0 ~f:( lor )
    ;;
  end
  external unix_mlockall : int -> unit = "unix_mlockall" ;;
  external unix_munlockall : unit -> unit = "unix_munlockall" ;;

  let mlockall flags = unix_mlockall (Mcl_flags.int_of_t_list flags) ;;
  let munlockall = unix_munlockall ;;
end ;;

(* end-of-pp    This delimiter stops the preprocessor being run on the below. *)

module File_descr = struct
  module M = struct
    type t = Unix.file_descr
    type sexpable = t
    let of_int n = file_descr_of_int n
    let to_int t = int_of_file_descr t
    let to_string t = Int.to_string (to_int t)
    let sexp_of_t t = Int.sexp_of_t (to_int t)
    let t_of_sexp _ = failwith "File_descr.t_of_sexp"
    let hash t = Int.hash (to_int t)
    let compare t1 t2 = Int.compare (to_int t1) (to_int t2)
  end
  include M
  include Hashable.Make (M)
end



let failwithf = Core_printf.failwithf

let atom x = Sexp.Atom x
let list x = Sexp.List x

let record l =
  list (List.map l ~f:(fun (name, value) -> list [atom name; value]))

let rec improve__non_intr_loop f cnt =
  if cnt = 0 then
    failwith "Unix non-interruptible failed with EINTR too many times"
  else
    try
      f ()
    with Unix.Unix_error (Unix.EINTR, _, _) ->
      improve__non_intr_loop f (cnt - 1)

let improve ?(restart=false) f make_arg_sexps =
  try
    if restart then improve__non_intr_loop f 1_000 else f ()
  with
  | Unix.Unix_error (e, s, _) ->
      let buf = Buffer.create 100 in
      let fmt = Format.formatter_of_buffer buf in
      Format.pp_set_margin fmt 10000;
      Sexp.pp_hum fmt (record (make_arg_sexps ()));
      Format.pp_print_flush fmt ();
      let arg_str = Buffer.contents buf in
      raise (Unix.Unix_error (e, s, arg_str))
;;

let dirname_r filename = ("dirname", atom filename)
let filename_r filename = ("filename", atom filename)
let file_perm_r perm = ("perm", atom (Printf.sprintf "0o%o" perm))
let len_r len = ("len", Int.sexp_of_t len)
let uid_r uid = ("uid", Int.sexp_of_t uid)
let gid_r gid = ("gid", Int.sexp_of_t gid)
let fd_r fd = ("fd", File_descr.sexp_of_t fd)
let dir_handle_r handle =
  let fd =
    try File_descr.sexp_of_t (dirfd handle)
    with _ -> Int.sexp_of_t (-1)
  in
  ("dir_handle", fd)
;;

let unary make_r f = (); fun x -> improve (fun () -> f x) (fun () -> [make_r x])
let unary_fd f = unary fd_r f
let unary_filename f = unary filename_r f
let unary_dirname f = unary dirname_r f
let unary_dir_handle f = unary dir_handle_r f


type error =
Unix.error =
| E2BIG | EACCES | EAGAIN | EBADF | EBUSY | ECHILD | EDEADLK | EDOM | EEXIST
| EFAULT | EFBIG | EINTR | EINVAL | EIO | EISDIR | EMFILE | EMLINK
| ENAMETOOLONG | ENFILE | ENODEV | ENOENT | ENOEXEC | ENOLCK | ENOMEM | ENOSPC
| ENOSYS | ENOTDIR | ENOTEMPTY | ENOTTY | ENXIO | EPERM | EPIPE | ERANGE
| EROFS | ESPIPE | ESRCH | EXDEV | EWOULDBLOCK | EINPROGRESS | EALREADY
| ENOTSOCK | EDESTADDRREQ | EMSGSIZE | EPROTOTYPE | ENOPROTOOPT
| EPROTONOSUPPORT | ESOCKTNOSUPPORT | EOPNOTSUPP | EPFNOSUPPORT | EAFNOSUPPORT
| EADDRINUSE | EADDRNOTAVAIL | ENETDOWN | ENETUNREACH | ENETRESET
| ECONNABORTED | ECONNRESET | ENOBUFS | EISCONN | ENOTCONN | ESHUTDOWN
| ETOOMANYREFS | ETIMEDOUT | ECONNREFUSED | EHOSTDOWN | EHOSTUNREACH | ELOOP
| EOVERFLOW | EUNKNOWNERR of int
with sexp


exception Unix_error = Unix.Unix_error

external unix_error : int -> string -> string -> _ = "unix_error_stub"
let error_message = Unix.error_message
let handle_unix_error = Unix.handle_unix_error
let environment = Unix.environment

let putenv ~key ~data =
  improve (fun () -> Unix.putenv key data)
    (fun () -> [("key", atom key); ("data", atom data)])
;;

let unsetenv name =
  
  if String.contains name '\000' then
    raise (Unix_error (EINVAL,"unsetenv",name));
  unsetenv name

module Process_status = struct
  type t = [
  | `Exited of int
  | `Signaled of Signal.t
  | `Stopped of Signal.t
  ] with sexp

  let of_unix = function
    | Unix.WEXITED i -> `Exited i
    | Unix.WSIGNALED i -> `Signaled (Signal.of_caml_int i)
    | Unix.WSTOPPED i -> `Stopped (Signal.of_caml_int i)
  ;;

  let to_string_hum = function
    | `Exited i -> sprintf "exited with code %d" i
    | `Signaled s ->
        sprintf "died after receiving %s (signal number %d)"
          (Signal.to_string s) (Signal.to_system_int s)
    | `Stopped s ->
        sprintf "stopped by %s (signal number %d)"
          (Signal.to_string s) (Signal.to_system_int s)
  ;;

  let is_ok = function
    | `Exited 0 -> true
    | _ -> false
  ;;
end

type wait_flag =
  Unix.wait_flag =
| WNOHANG
| WUNTRACED
with sexp_of

let prog_r prog = ("prog", atom prog)
let args_r args = ("args", sexp_of_array atom args)
let env_r env = ("env", sexp_of_array atom env)

let execv ~prog ~args =
  improve (fun () -> Unix.execv ~prog ~args)
    (fun () -> [prog_r prog; args_r args])
;;

let execve ~prog ~args ~env =
  improve (fun () -> Unix.execve ~prog ~args ~env)
    (fun () -> [prog_r prog; args_r args; env_r env])
;;

let execvp ~prog ~args =
  improve (fun () -> Unix.execvp ~prog ~args)
    (fun () -> [prog_r prog; args_r args])
;;

let execvpe ~prog ~args ~env =
  improve (fun () -> Unix.execvpe ~prog ~args ~env)
    (fun () -> [prog_r prog; args_r args; env_r env])
;;

let fork = Unix.fork

let wait ?restart () =
  improve ?restart
    (fun () ->
       let x, ps = Unix.wait () in
       (x, Process_status.of_unix ps))
    (fun () -> [])
;;

let waitpid ?restart ~mode pid =
  improve ?restart
    (fun () ->
       let x, ps = Unix.waitpid ~mode pid in
       (x, Process_status.of_unix ps))
    (fun () ->
      [("mode", sexp_of_list sexp_of_wait_flag mode);
       ("pid", Int.sexp_of_t pid)])
;;

let system s =
  improve (fun () -> Process_status.of_unix (Unix.system s))
          (fun () -> [("command", atom s)])
;;

let getpid = Unix.getpid
let getppid = Unix.getppid

let nice i =
  improve (fun () -> Unix.nice i)
    (fun () -> [("priority", Int.sexp_of_t i)])
;;

type file_descr = Unix.file_descr

let stdin = Unix.stdin
let stdout = Unix.stdout
let stderr = Unix.stderr

type open_flag =
Unix.open_flag =
| O_RDONLY
| O_WRONLY
| O_RDWR
| O_NONBLOCK
| O_APPEND
| O_CREAT
| O_TRUNC
| O_EXCL
| O_NOCTTY
| O_DSYNC
| O_SYNC
| O_RSYNC
with sexp_of

type file_perm = int with of_sexp

(* Prints out in octal, which is much more standard in Unix. *)
let sexp_of_file_perm fp = Sexp.Atom (Printf.sprintf "0o%03o" fp)

let is_rw_open_flag = function O_RDONLY | O_WRONLY | O_RDWR -> true | _ -> false

let openfile filename ~mode ~perm =
  let mode_sexp () = sexp_of_list sexp_of_open_flag mode in
  if not (Core_list.exists mode ~f:is_rw_open_flag) then
    failwithf "Unix.openfile: no read or write flag specified in mode: %s"
      (Sexp.to_string (mode_sexp ())) ()
  else
    improve (fun () -> Unix.openfile filename ~mode ~perm)
      (fun () -> [filename_r filename;
                  ("mode", mode_sexp ());
                  file_perm_r perm])
;;

let close = unary_fd Unix.close

let read_write f fd ~buf ~pos ~len =
  improve (fun () -> f fd ~buf ~pos ~len)
    (fun () -> [fd_r fd; ("pos", Int.sexp_of_t pos); len_r len])
;;

let read = read_write Unix.read
let write = read_write Unix.write
let single_write = read_write Unix.single_write

let in_channel_of_descr = Unix.in_channel_of_descr
let out_channel_of_descr = Unix.out_channel_of_descr
let descr_of_in_channel = Unix.descr_of_in_channel
let descr_of_out_channel = Unix.descr_of_out_channel

#undef SEEK_SET
#undef SEEK_CUR
#undef SEEK_END
type seek_command =
Unix.seek_command =
| SEEK_SET
| SEEK_CUR
| SEEK_END
with sexp_of

type file_kind = Unix.file_kind =
| S_REG
| S_DIR
| S_CHR
| S_BLK
| S_LNK
| S_FIFO
| S_SOCK
with sexp

let isatty = unary_fd Unix.isatty

module Native_file = struct
  type stats =
  Unix.stats = {
    st_dev : int;
    st_ino : int;
    st_kind : file_kind;
    st_perm : file_perm;
    st_nlink : int;
    st_uid : int;
    st_gid : int;
    st_rdev : int;
    st_size : int;
    st_atime : float;
    st_mtime : float;
    st_ctime : float;
  } with sexp

  let stat = unary_filename Unix.stat
  let lstat = unary_filename Unix.lstat
  let fstat = unary_fd Unix.fstat

  let lseek fd pos ~mode =
    improve (fun () -> Unix.lseek fd pos ~mode)
      (fun () -> [fd_r fd;
                  ("pos", Int.sexp_of_t pos);
                  ("mode", sexp_of_seek_command mode)])
  ;;

  let truncate filename ~len =
    improve (fun () -> Unix.truncate filename ~len)
      (fun () -> [filename_r filename; len_r len])
  ;;

  let ftruncate fd ~len =
    improve (fun () -> Unix.ftruncate fd ~len)
      (fun () -> [fd_r fd; len_r len])
  ;;
end

open LargeFile

#undef F_ULOCK
#undef F_LOCK
#undef F_TLOCK
#undef F_TEST
#undef F_RLOCK
#undef F_TRLOCK
type lock_command =
  Unix.lock_command =
  | F_ULOCK
  | F_LOCK
  | F_TLOCK
  | F_TEST
  | F_RLOCK
  | F_TRLOCK
  with sexp_of

let lockf fd ~mode ~len =
  let len =
    try Core_int64.to_int_exn len with _ ->
      failwith "~len passed to Unix.lockf too large to fit in native int"
  in
  improve (fun () -> Unix.lockf fd ~mode ~len)
    (fun () -> [fd_r fd;
                ("mode", sexp_of_lock_command mode);
                len_r len])
;;

module Flock_command = struct
  

  (* These must never be reordered, the underlying C call relies on this order *)
  type t =
    | LOCK_SH
    | LOCK_EX
    | LOCK_UN
end

external flock : file_descr -> Flock_command.t -> bool = "core_unix_flock"

let lseek fd pos ~mode =
  improve (fun () -> Unix.LargeFile.lseek fd pos ~mode)
    (fun () -> [fd_r fd;
                ("pos", Int64.sexp_of_t pos);
                ("mode", sexp_of_seek_command mode)])
;;

let len64_r len = ("len", Int64.sexp_of_t len)

let truncate filename ~len =
  improve (fun () -> Unix.LargeFile.truncate filename ~len)
    (fun () -> [filename_r filename; len64_r len])
;;

let ftruncate fd ~len =
  improve (fun () -> Unix.LargeFile.ftruncate fd ~len)
    (fun () -> [fd_r fd; len64_r len])
;;

type stats =
Unix.LargeFile.stats = {
  st_dev : int;
  st_ino : int;
  st_kind : file_kind;
  st_perm : file_perm;
  st_nlink : int;
  st_uid : int;
  st_gid : int;
  st_rdev : int;
  st_size : int64;
  st_atime : float;
  st_mtime : float;
  st_ctime : float;
} with sexp


external stat : string -> stats = "core_unix_stat_64"
let stat = unary_filename stat

external lstat : string -> stats = "core_unix_lstat_64"
let lstat = unary_filename lstat

external fstat : file_descr -> stats = "core_unix_fstat_64"
let fstat = unary_fd fstat

let src_dst f ~src ~dst =
  improve (fun () -> f ~src ~dst)
    (fun () -> [("src", atom src); ("dst", atom dst)])
;;

let unlink = unary_filename Unix.unlink
let rename = src_dst Unix.rename

let link ?(force = false) ~target ~link_name () =
  improve
    (fun () ->
      if force then begin
        try Unix.unlink link_name
        with Unix_error (Unix.ENOENT, _, _) -> ()
      end;
      Unix.link ~src:target ~dst:link_name)
    (fun () -> [("target", atom target); ("link_name", atom link_name)])

#undef R_OK
#undef W_OK
#undef X_OK
#undef F_OK
type access_permission = Unix.access_permission =
  | R_OK
  | W_OK
  | X_OK
  | F_OK
with sexp_of

let chmod filename ~perm =
  improve (fun () -> Unix.chmod filename ~perm)
    (fun () -> [filename_r filename; file_perm_r perm])
;;

let fchmod fd ~perm =
  improve (fun () -> Unix.fchmod fd ~perm)
    (fun () -> [fd_r fd; file_perm_r perm])
;;

let chown filename ~uid ~gid =
  improve (fun () -> Unix.chown filename ~uid ~gid)
    (fun () -> [filename_r filename; uid_r uid; gid_r gid])
;;

let fchown fd ~uid ~gid =
  improve (fun () -> Unix.fchown fd ~uid ~gid)
    (fun () -> [fd_r fd; uid_r uid; gid_r gid])
;;

let umask mode =
  improve (fun () -> Unix.umask mode)
    (fun () -> [("mode", atom (Printf.sprintf "0o%o" mode))])
;;

let access filename ~perm =
  improve (fun () -> Unix.access filename ~perm)
    (fun () -> [filename_r filename;
                ("perm", sexp_of_list sexp_of_access_permission perm)])
;;

let dup = unary_fd Unix.dup

let dup2 ~src ~dst =
  improve (fun () -> Unix.dup2 ~src ~dst)
    (fun () -> [("src", File_descr.sexp_of_t src);
                ("dst", File_descr.sexp_of_t dst)])
;;

let set_nonblock = unary_fd Unix.set_nonblock
let clear_nonblock = unary_fd Unix.clear_nonblock
let set_close_on_exec = unary_fd Unix.set_close_on_exec
let clear_close_on_exec = unary_fd Unix.clear_close_on_exec

let mkdir dirname ~perm =
  improve (fun () -> Unix.mkdir dirname ~perm)
    (fun () -> [dirname_r dirname; file_perm_r perm])
;;
let rmdir = unary_dirname Unix.rmdir
let chdir = unary_dirname Unix.chdir
let getcwd = Unix.getcwd
let chroot = unary_dirname Unix.chroot

type dir_handle = Unix.dir_handle

let opendir = unary_dirname Unix.opendir
let readdir = unary_dir_handle Unix.readdir
let rewinddir = unary_dir_handle Unix.rewinddir
(* if closedir is passed an already closed file handle it will try to call
  dirfd on it to get a file descriptor for the error message, which will fail
  with invalid argument because closedir sets the fd to null *)
let closedir =
  unary_dir_handle (fun dh ->
    try Unix.closedir dh with | Invalid_argument _ -> ())

let pipe = Unix.pipe

let mkfifo name ~perm =
  improve (fun () -> Unix.mkfifo name ~perm)
    (fun () -> [("name", atom name); file_perm_r perm])
;;


module Process_info = struct
  (* Any change to the order of these fields must be accompanied by a
     corresponding change to unix_stubs.c:ml_create_process. *)
  type t = {
    pid : int;
    stdin : file_descr;
    stdout : file_descr;
    stderr : file_descr;
  }
end

external create_process :
  prog : string ->
  args : string array ->
  env : string array ->
  search_path : bool ->
  Process_info.t
  = "ml_create_process"

type env = [
  | `Replace of (string * string) list
  | `Extend of (string * string) list
] with sexp

let create_process_env ~prog ~args ~(env:env) =
  let module Map = Core_map in
  let env_map =
    let current, env =
      match env with
      | `Replace env -> [], env
      | `Extend env ->
          List.map (Array.to_list (Unix.environment ()))
            ~f:(fun s -> String.lsplit2_exn s ~on:'='), env
    in
    List.fold_left (current @ env) ~init:Map.empty
      ~f:(fun map (key, data) -> Map.add map ~key ~data)
  in
  let env =
    Map.fold env_map ~init:[]
      ~f:(fun ~key ~data acc -> (key ^ "=" ^ data) :: acc)
  in
  create_process
    ~search_path:true
    ~prog
    ~args:(Array.of_list args)
    ~env:(Array.of_list env)

let create_process_env ~prog ~args ~env =
  improve (fun () -> create_process_env ~prog ~args ~env)
    (fun () ->
      [("prog", atom prog);
       ("args", sexp_of_list atom args);
       ("env", sexp_of_env env)])


(* A lot was just stolen Unix.open_process_full *)
let create_process ~prog ~args =
  improve (fun () -> create_process_env ~prog ~args ~env:(`Extend []))
    (fun () ->
      [("prog", atom prog);
       ("args", sexp_of_list atom args)])

let make_open_process f command =
  improve (fun () -> f command)
    (fun () -> [("command", atom command)])

let open_process_in = make_open_process Unix.open_process_in
let open_process_out = make_open_process Unix.open_process_out
let open_process = make_open_process Unix.open_process

module Process_channels = struct
  type t = {
    stdin : out_channel;
    stdout : in_channel;
    stderr : in_channel;
  }
end

let open_process_full command ~env =
  improve (fun () ->
    let stdout, stdin, stderr = Unix.open_process_full command ~env in
    { Process_channels.stdin = stdin; stdout = stdout; stderr = stderr })
    (fun () -> [("command", atom command);
                ("env", sexp_of_array atom env)])
;;

let close_process_in ic = Process_status.of_unix (Unix.close_process_in ic)
let close_process_out oc = Process_status.of_unix (Unix.close_process_out oc)

let close_process (ic, oc) =
  Process_status.of_unix (Unix.close_process (ic, oc))

let close_process_full c =
  let module C = Process_channels in
  Process_status.of_unix
    (Unix.close_process_full (c.C.stdout, c.C.stdin, c.C.stderr))

let symlink = src_dst Unix.symlink
let readlink = unary_filename Unix.readlink

module Select_fds = struct
  type t = {
    read : file_descr list;
    write : file_descr list;
    except : file_descr list;
  }

  let empty = { read = []; write = []; except = [] }
end


let select ?restart ~read ~write ~except ~timeout () =
  improve ?restart (fun () ->
    let read, write, except = Unix.select ~read ~write ~except ~timeout in
    { Select_fds.read = read; write = write; except = except })
    (fun () ->
      [("read", sexp_of_list File_descr.sexp_of_t read);
       ("write", sexp_of_list File_descr.sexp_of_t write);
       ("except", sexp_of_list File_descr.sexp_of_t except);
       ("timeout", sexp_of_float timeout)])
;;

let pause = Unix.pause

type process_times =
Unix.process_times = {
  tms_utime : float;
  tms_stime : float;
  tms_cutime : float;
  tms_cstime : float;
}

type tm =
Unix.tm = {
  tm_sec : int;
  tm_min : int;
  tm_hour : int;
  tm_mday : int;
  tm_mon : int;
  tm_year : int;
  tm_wday : int;
  tm_yday : int;
  tm_isdst : bool;
}

let time = Unix.time
let gettimeofday = Unix.gettimeofday

external localtime : float -> Unix.tm = "core_localtime"
external gmtime : float -> Unix.tm = "core_gmtime"
external timegm : Unix.tm -> float = "core_timegm" (* the inverse of gmtime *)

let mktime = Unix.mktime
let alarm = Unix.alarm
let sleep = Unix.sleep
let times = Unix.times
let utimes = Unix.utimes
external strftime : tm -> string -> string = "unix_strftime"

type interval_timer = Unix.interval_timer =
  | ITIMER_REAL
  | ITIMER_VIRTUAL
  | ITIMER_PROF

type interval_timer_status = Unix.interval_timer_status = {
  it_interval : float;
  it_value : float;
}

let getitimer = Unix.getitimer
let setitimer = Unix.setitimer

let getuid = Unix.getuid
let geteuid = Unix.geteuid

let setuid uid =
  improve (fun () -> Unix.setuid uid)
    (fun () -> [("uid", Int.sexp_of_t uid)])

let getgid = Unix.getgid
let getegid = Unix.getegid

let setgid gid =
  improve (fun () -> Unix.setgid gid)
    (fun () -> [("gid", Int.sexp_of_t gid)])

let getgroups = Unix.getgroups

let make_by f make_exn =
  let normal arg = try Some (f arg) with Not_found -> None in
  let exn arg = try f arg with Not_found -> raise (make_exn arg) in
  (normal, exn)
;;

module Passwd = struct
  type t =
    { name : string;
      passwd : string;
      uid : int;
      gid : int;
      gecos : string;
      dir : string;
      shell : string;
    }

  let of_unix u =
    { name = u.Unix.pw_name;
      passwd = u.Unix.pw_passwd;
      uid = u.Unix.pw_uid;
      gid = u.Unix.pw_gid;
      gecos = u.Unix.pw_gecos;
      dir = u.Unix.pw_dir;
      shell = u.Unix.pw_shell;
    }
  ;;

  exception Getbyname of string with sexp

  let (getbyname, getbyname_exn) =
    make_by (fun name -> of_unix (Unix.getpwnam name))
      (fun s -> Getbyname s)
  ;;

  exception Getbyuid of int with sexp

  let (getbyuid, getbyuid_exn) =
    make_by (fun uid -> of_unix (Unix.getpwuid uid)) (fun s -> Getbyuid s)
  ;;

  exception Getpwent with sexp

  external core_setpwent : unit -> unit = "core_setpwent" ;;
  external core_endpwent : unit -> unit = "core_endpwent" ;;
  external core_getpwent : unit -> Unix.passwd_entry = "core_getpwent" ;;

  let setpwent = core_setpwent ;;
  let endpwent = core_endpwent ;;

  let getpwent_exn () = of_unix (core_getpwent ()) ;;
  let getpwent () = Option.try_with (fun () -> getpwent_exn ()) ;;
end

module Group = struct
  type t =
    { name : string;
      passwd : string;
      gid : int;
      mem : string array;
    }
  with sexp_of

  let of_unix u =
    { name = u.Unix.gr_name;
      passwd = u.Unix.gr_passwd;
      gid = u.Unix.gr_gid;
      mem = u.Unix.gr_mem;
    }
  ;;

  exception Getbyname of string with sexp

  let (getbyname, getbyname_exn) =
    make_by (fun name -> of_unix (Unix.getgrnam name)) (fun s -> Getbyname s)
  ;;

  exception Getbygid of int with sexp

  let (getbygid, getbygid_exn) =
    make_by (fun gid -> of_unix (Unix.getgrgid gid)) (fun s -> Getbygid s)
  ;;
end

(* The standard getlogin function goes through utmp which is unreliable,
   see the BUGS section of getlogin(3) *)
let getlogin_orig = Unix.getlogin
let getlogin () = (Unix.getpwuid (getuid ())).Unix.pw_name

module Inet_addr = struct
  type t = Unix.inet_addr

  include Binable.Of_stringable (struct
    type stringable = t
    let of_string = Unix.inet_addr_of_string
    let to_string = Unix.string_of_inet_addr
  end)

  let of_string = Unix.inet_addr_of_string
  let to_string = Unix.string_of_inet_addr
end

type inet_addr = Inet_addr.t with bin_io

let inet_addr_of_string = Unix.inet_addr_of_string
let string_of_inet_addr = Unix.string_of_inet_addr

let sexp_of_inet_addr addr = Sexp.Atom (Inet_addr.to_string addr)

let gethostname = Unix.gethostname

module Host = struct
  type t =
    { name : string;
      aliases : string array;
      addrtype : Unix.socket_domain;
      addrs : inet_addr array;
    }

  let of_unix u =
    { name = u.Unix.h_name;
      aliases = u.Unix.h_aliases;
      addrtype = u.Unix.h_addrtype;
      addrs = u.Unix.h_addr_list;
    }

  exception Getbyname of string with sexp

  let (getbyname, getbyname_exn) =
    make_by (fun name -> of_unix (Unix.gethostbyname name))
      (fun s -> Getbyname s)
  ;;

  exception Getbyaddr of inet_addr with sexp

  let (getbyaddr, getbyaddr_exn) =
    make_by (fun addr -> of_unix (Unix.gethostbyaddr addr))
      (fun a -> Getbyaddr a)
  ;;
end

module Protocol = struct
  type t =
    { name : string;
      aliases : string array;
      proto : int;
    }

  let of_unix u =
    { name = u.Unix.p_name;
      aliases = u.Unix.p_aliases;
      proto = u.Unix.p_proto;
    }

  exception Getbyname of string with sexp
  let (getbyname, getbyname_exn) =
    make_by (fun name -> of_unix (Unix.getprotobyname name))
      (fun s -> Getbyname s)
  ;;

  exception Getbynumber of int with sexp
  let (getbynumber, getbynumber_exn) =
    make_by (fun i -> of_unix (Unix.getprotobynumber i))
      (fun i -> Getbynumber i)
  ;;
end

module Service = struct
  type t =
    { name : string;
      aliases : string array;
      port : int;
      proto : string;
    }

  let of_unix u =
    { name = u.Unix.s_name;
      aliases = u.Unix.s_aliases;
      port = u.Unix.s_port;
      proto = u.Unix.s_proto;
    }

  exception Getbyname of string * string with sexp

  let getbyname_exn name ~protocol =
    try of_unix (Unix.getservbyname name ~protocol)
    with Not_found -> raise (Getbyname (name, protocol))
  ;;

  let getbyname name ~protocol =
    try Some (of_unix (Unix.getservbyname name ~protocol))
    with _ -> None
  ;;

  exception Getbyport of int * string with sexp

  let getbyport_exn num ~protocol =
    try of_unix (Unix.getservbyport num ~protocol)
    with Not_found -> raise (Getbyport (num, protocol))
  ;;

  let getbyport num ~protocol =
    try Some (of_unix (Unix.getservbyport num ~protocol))
    with Not_found -> None
  ;;
end

exception Get_inet_addr of string * string with sexp

#undef PF_UNIX
#undef PF_INET
#undef PF_INET6
type socket_domain = Unix.socket_domain =
  | PF_UNIX
  | PF_INET
  | PF_INET6
with sexp, bin_io

let get_inet_addr name =
  try inet_addr_of_string name
  with Failure _ ->
    match Host.getbyname name with
    | None -> raise (Get_inet_addr (name, "host not found"))
    | Some host ->
        match host.Host.addrtype with
        | Unix.PF_INET | Unix.PF_INET6 ->
            let addrs = host.Host.addrs in
            if Array.length addrs > 0 then addrs.(0)
            else raise (Get_inet_addr (name, "empty addrs"))
        | Unix.PF_UNIX -> assert false  (* impossible *)

let inet_addr_of_sexp = function
  | Sexp.List _ as sexp ->
      of_sexp_error "Core.Core_unix.inet_addr_of_sexp: atom expected" sexp
  | Sexp.Atom name -> get_inet_addr name

let inet_addr_any = Unix.inet_addr_any
let inet_addr_loopback = Unix.inet_addr_loopback
let inet6_addr_any = Unix.inet6_addr_any
let inet6_addr_loopback = Unix.inet6_addr_loopback

#undef SOCK_STREAM
#undef SOCK_DGRAM
#undef SOCK_RAW
#undef SOCK_SEQPACKET
type socket_type = Unix.socket_type =
  | SOCK_STREAM
  | SOCK_DGRAM
  | SOCK_RAW
  | SOCK_SEQPACKET
with sexp, bin_io

type sockaddr = Unix.sockaddr =
  | ADDR_UNIX of string
  | ADDR_INET of inet_addr * int
with sexp, bin_io

let domain_of_sockaddr = Unix.domain_of_sockaddr

let addr_r addr = ("addr", sexp_of_sockaddr addr)

let socket_or_pair f ~domain ~kind ~protocol =
  improve (fun () -> f ~domain ~kind ~protocol)
    (fun () -> [("domain", sexp_of_socket_domain domain);
                ("kind", sexp_of_socket_type kind);
                ("protocol", Int.sexp_of_t protocol)])
;;

let socket = socket_or_pair Unix.socket
let socketpair = socket_or_pair Unix.socketpair

let accept = unary_fd Unix.accept

let bind fd ~addr =
  improve (fun () -> Unix.bind fd ~addr)
    (fun () -> [fd_r fd; addr_r addr])
;;

let connect fd ~addr =
  improve (fun () -> Unix.connect fd ~addr)
    (fun () -> [fd_r fd; addr_r addr])
;;

let listen fd ~max =
  improve (fun () -> Unix.listen fd ~max)
    (fun () -> [fd_r fd; ("max", Int.sexp_of_t max)])
;;

type shutdown_command = Unix.shutdown_command =
  | SHUTDOWN_RECEIVE
  | SHUTDOWN_SEND
  | SHUTDOWN_ALL
with sexp_of

let shutdown fd ~mode =
  improve (fun () ->
    try
      Unix.shutdown fd ~mode
    with
    (* the error below is benign, it means that the other side disconnected *)
    | Unix.Unix_error (Unix.ENOTCONN, _, _) -> ())
    (fun () -> [fd_r fd; ("mode", sexp_of_shutdown_command mode)])
;;

let getsockname = unary_fd Unix.getsockname

let getpeername = unary_fd Unix.getpeername

#undef MSG_OOB
#undef MSG_DONTROUTE
#undef MSG_PEEK
type msg_flag =
Unix.msg_flag =
| MSG_OOB
| MSG_DONTROUTE
| MSG_PEEK
with sexp_of

let recv_send f fd ~buf ~pos ~len ~mode =
  improve (fun () -> f fd ~buf ~pos ~len ~mode)
    (fun () ->
      [fd_r fd;
       ("pos", Int.sexp_of_t pos);
       len_r len;
       ("mode", sexp_of_list sexp_of_msg_flag mode)])
;;

let recv = recv_send Unix.recv
let recvfrom = recv_send Unix.recvfrom
let send = recv_send Unix.send

let sendto fd ~buf ~pos ~len ~mode ~addr =
  improve (fun () -> Unix.sendto fd ~buf ~pos ~len ~mode ~addr)
    (fun () ->
      [fd_r fd;
       ("pos", Int.sexp_of_t pos);
       len_r len;
       ("mode", sexp_of_list sexp_of_msg_flag mode);
       ("addr", sexp_of_sockaddr addr)])
;;

#undef SO_DEBUG
#undef SO_BROADCAST
#undef SO_REUSEADDR
#undef SO_KEEPALIVE
#undef SO_DONTROUTE
#undef SO_OOBINLINE
#undef SO_ACCEPTCONN
type socket_bool_option = Unix.socket_bool_option =
  | SO_DEBUG
  | SO_BROADCAST
  | SO_REUSEADDR
  | SO_KEEPALIVE
  | SO_DONTROUTE
  | SO_OOBINLINE
  | SO_ACCEPTCONN
  | TCP_NODELAY
  | IPV6_ONLY
with sexp_of

#undef SO_SNDBUF
#undef SO_RCVBUF
#undef SO_ERROR
#undef SO_TYPE
#undef SO_RCVLOWAT
#undef SO_SNDLOWAT
type socket_int_option = Unix.socket_int_option =
  | SO_SNDBUF
  | SO_RCVBUF
  | SO_ERROR
  | SO_TYPE
  | SO_RCVLOWAT
  | SO_SNDLOWAT
with sexp_of

#undef SO_LINGER
type socket_optint_option = Unix.socket_optint_option =
  | SO_LINGER
with sexp_of

#undef SO_RCVTIMEO
#undef SO_SNDTIMEO
type socket_float_option = Unix.socket_float_option =
  | SO_RCVTIMEO
  | SO_SNDTIMEO
with sexp_of

let make_sockopt get set sexp_of_opt sexp_of_val =
  let getsockopt fd opt =
    improve (fun () -> get fd opt)
      (fun () -> [fd_r fd; ("opt", sexp_of_opt opt)])
  in
  let setsockopt fd opt value =
    improve (fun () -> set fd opt value)
      (fun () ->
        [fd_r fd; ("opt", sexp_of_opt opt); ("val", sexp_of_val value)])
  in
  (getsockopt, setsockopt)
;;

let (getsockopt, setsockopt) =
  make_sockopt Unix.getsockopt Unix.setsockopt
    sexp_of_socket_bool_option sexp_of_bool
;;

let (getsockopt_int, setsockopt_int) =
  make_sockopt Unix.getsockopt_int Unix.setsockopt_int
    sexp_of_socket_int_option sexp_of_int
;;

let (getsockopt_optint, setsockopt_optint) =
  make_sockopt Unix.getsockopt_optint Unix.setsockopt_optint
    sexp_of_socket_optint_option (sexp_of_option sexp_of_int)
;;

let (getsockopt_float, setsockopt_float) =
  make_sockopt Unix.getsockopt_float Unix.setsockopt_float
    sexp_of_socket_float_option sexp_of_float
;;


let open_connection addr =
  improve (fun () -> Unix.open_connection addr) (fun () -> [addr_r addr])
;;

let shutdown_connection = Unix.shutdown_connection

let establish_server handle_connection ~addr =
  improve (fun () -> Unix.establish_server handle_connection ~addr)
    (fun () -> [addr_r addr])
;;

type addr_info = Unix.addr_info = {
  ai_family : socket_domain;
  ai_socktype : socket_type;
  ai_protocol : int;
  ai_addr : sockaddr;
  ai_canonname : string;
} with sexp_of

type getaddrinfo_option = Unix.getaddrinfo_option =
  | AI_FAMILY of socket_domain
  | AI_SOCKTYPE of socket_type
  | AI_PROTOCOL of int
  | AI_NUMERICHOST
  | AI_CANONNAME
  | AI_PASSIVE
with sexp_of

let getaddrinfo host service opts =
  improve (fun () -> Unix.getaddrinfo host service opts)
    (fun () ->
      [("host", atom host);
       ("service", atom service);
       ("opts", sexp_of_list sexp_of_getaddrinfo_option opts)])
;;

type name_info =
Unix.name_info = {
  ni_hostname : string;
  ni_service : string;
}

type getnameinfo_option =
Unix.getnameinfo_option =
| NI_NOFQDN
| NI_NUMERICHOST
| NI_NAMEREQD
| NI_NUMERICSERV
| NI_DGRAM
with sexp_of

let getnameinfo addr opts =
  improve (fun () -> Unix.getnameinfo addr opts)
    (fun () ->
      [("addr", sexp_of_sockaddr addr);
       ("opts", sexp_of_list sexp_of_getnameinfo_option opts)])
;;

external get_terminal_size : unit -> int * int = "unix_get_terminal_size"

module Terminal_io = struct
  type t = Unix.terminal_io = {
    mutable c_ignbrk : bool;
    mutable c_brkint : bool;
    mutable c_ignpar : bool;
    mutable c_parmrk : bool;
    mutable c_inpck : bool;
    mutable c_istrip : bool;
    mutable c_inlcr : bool;
    mutable c_igncr : bool;
    mutable c_icrnl : bool;
    mutable c_ixon : bool;
    mutable c_ixoff : bool;
    mutable c_opost : bool;
    mutable c_obaud : int;
    mutable c_ibaud : int;
    mutable c_csize : int;
    mutable c_cstopb : int;
    mutable c_cread : bool;
    mutable c_parenb : bool;
    mutable c_parodd : bool;
    mutable c_hupcl : bool;
    mutable c_clocal : bool;
    mutable c_isig : bool;
    mutable c_icanon : bool;
    mutable c_noflsh : bool;
    mutable c_echo : bool;
    mutable c_echoe : bool;
    mutable c_echok : bool;
    mutable c_echonl : bool;
    mutable c_vintr : char;
    mutable c_vquit : char;
    mutable c_verase : char;
    mutable c_vkill : char;
    mutable c_veof : char;
    mutable c_veol : char;
    mutable c_vmin : int;
    mutable c_vtime : int;
    mutable c_vstart : char;
    mutable c_vstop : char;
  } with sexp_of

  let tcgetattr = unary_fd Unix.tcgetattr

  type setattr_when = Unix.setattr_when =
    | TCSANOW
    | TCSADRAIN
    | TCSAFLUSH
  with sexp_of

  let tcsetattr fd ~mode termios =
    improve (fun () -> Unix.tcsetattr fd ~mode termios)
      (fun () -> [fd_r fd;
                  ("mode", sexp_of_setattr_when mode);
                  ("termios", sexp_of_t termios)])
  ;;

  let tcsendbreak fd ~duration =
    improve (fun () -> Unix.tcsendbreak fd ~duration)
      (fun () -> [fd_r fd;
                  ("duration", Int.sexp_of_t duration)])
  ;;

  let tcdrain = unary_fd Unix.tcdrain

  type flush_queue = Unix.flush_queue =
    | TCIFLUSH
    | TCOFLUSH
    | TCIOFLUSH
  with sexp_of

  let tcflush fd ~mode =
    improve (fun () -> Unix.tcflush fd ~mode)
      (fun () -> [fd_r fd; ("mode", sexp_of_flush_queue mode)])
  ;;

  type flow_action = Unix.flow_action =
    | TCOOFF
    | TCOON
    | TCIOFF
    | TCION
  with sexp_of

  let tcflow fd ~mode =
    improve (fun () -> Unix.tcflow fd ~mode)
      (fun () -> [fd_r fd; ("mode", sexp_of_flow_action mode)])
  ;;

  let setsid = Unix.setsid
end

let get_sockaddr name port = ADDR_INET (get_inet_addr name, port)

let set_in_channel_timeout ic rcv_timeout =
  let s = descr_of_in_channel ic in
  setsockopt_float s SO_RCVTIMEO rcv_timeout

let set_out_channel_timeout oc snd_timeout =
  let s = descr_of_out_channel oc in
  setsockopt_float s SO_SNDTIMEO snd_timeout

(* vim: set filetype=ocaml : *)

