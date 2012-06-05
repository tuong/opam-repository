(******************************************************************************
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
 ******************************************************************************)

include Extended_common

module Array = struct
  include Core.Std.Array
  include Extended_array
end
module Atomic_edit = Atomic_edit
module Bin_io_utils = Bin_io_utils
module Bitarray = Bitarray
module Cache = Cache
module Cbuffer = Cbuffer
module Command = Command
module Console = Console
module Container = struct
  include Core.Std.Container
  include Extended_container
end
module Csv_writer = Csv_writer
module Date = struct
  include Core.Std.Date
  include Extended_time.Extended_date
end
module English = English
module Documented_match_statement = Documented_match_statement
module Exception_check = Exception_check
module Exn = struct
  include Core.Std.Exn
  include Extended_exn
end
module Extra_fields = Extra_fields
module Fd_leak_check = Fd_leak_check
module Filename = struct
  include Core.Std.Filename
  include Extended_filename
end
module Find = Find
module Fold_map = Fold_map
module Float = struct
  include Core.Float
  include Extended_float
end
module Fn = struct
  include Core.Std.Fn
  include Extended_fn
end
module Gc = struct
  include Core.Std.Gc
  include Extended_gc
end
module Int = struct
  include Core.Std.Int
  include Extended_int
end
module Int32 = struct
  include Core.Std.Int32
  include Extended_int32
end
module Int63 = struct
  include Core.Std.Int63
  include Extended_int63
end
module Int64 = struct
  include Core.Std.Int64
  include Extended_int64
end
module Low_level_debug = Low_level_debug
module Nativeint = struct
  include Core.Std.Nativeint
  include Extended_nativeint
end
module Number = Number
module Thread = struct
  include Thread
  include Extended_thread
end
module Timed_function = Timed_function
module Iter = Iter
module Lazy_list = Lazy_list
module Lazy_m = Lazy_m
#if !defined(JSC_NO_LINUX_EXT)
module Linux_ext = struct
  include Core.Std.Linux_ext
  include Extended_linux
end
#else
#warning "linux_ext not supported, not being included in Core_extended.Std"
#endif

module List = struct
  include Core.Std.List
  include Extended_list
end
module Logger = Logger
module Loggers = Loggers
module Memo = struct
  include Core.Std.Memo
  include Extended_memo
end
module Multi_map = Multi_map
module Net_utils = Net_utils
module Pp = Pp
module Process = Process
module Procfs = Procfs
module Readline = Readline
module Result = struct
  include Core.Std.Result
  include Extended_result
end
module Runtime_blockout_detector = Runtime_blockout_detector
module Rw_mutex = Rw_mutex
module Semaphore = Semaphore
module Sendmail = Sendmail
module Sexp = struct
  include Core.Std.Sexp
  include Extended_sexp
end
module Shell = Shell
module String = struct
  include Core.Std.String
  include Extended_string
end
module Sys = struct
  include Core.Std.Sys
  include Extended_sys
end
module Syslog = Syslog
module Sys_utils = Sys_utils
module Time = struct
  include Core.Std.Time
  include Extended_time
end
module Search = Search_foo
module Union_find = Union_find
module Unix = struct
  include Core.Core_unix
  include Extended_unix
end
module Update_queue = Update_queue
module Printc = Printc
