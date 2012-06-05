(******************************************************************************
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
 ******************************************************************************)

(* We list the modules we want to export here and follow the convention of opening
   Core.Std instead of Core. *)

include Std_internal

module Time = struct
  module Date = Date
  module Span = Span

  module Ofday = struct
    include Ofday

    (* This helper function is used more than 30 times in the base tree, but can't be
       defined in Ofday directly because it would create a circular reference *)
    let now () = snd (Time.to_date_ofday (Time.now ()))
  end

  include Time
end

(* Included here instead of in common because time depends on common *)
include Time.Date.Export

(* Can't go in Common for circular-reference reasons *)
let sec = Time.Span.of_sec

module Agnostic_mutex = Agnostic_mutex
module Arg = Core_arg
module Bag = Bag
module Bigbuffer = Bigbuffer
module Bigstring = Bigstring
module Bigsubstring = Bigsubstring
module Binable = Binable
#if defined(__linux__) && !defined(JSC_NO_LINUX_EXT)
module Linux_ext = Linux_ext
module Bigstring_marshal = Bigstring_marshal
#else
#warning "linux_ext not supported, not being included in Core.Std"
#endif
module Binary_packing = Binary_packing
module Blang = Blang
module Bucket = Bucket
module Byte_units = Byte_units
module Caml = Caml
module Comparable = Comparable
module Condition = Core_condition
module Container = Container
module Crc = Crc
module Date = Date
module Daemon = Daemon
module Dequeue = Dequeue
module Doubly_linked = Doubly_linked
module Error_check = Error_check
module Exn = Exn
module Float = Float
module Float_intf = Float_intf
module Force_once = Force_once
module Fqueue = Fqueue
module Filename = Core_filename
module Floatable = Floatable
module Fn = Fn
module Gc = Core_gc
module Hash_queue = Hash_queue
module Hash_heap = Hash_heap
module Hash_set = Hash_set
module Hashable = Hashable
module Heap = Heap
module Identifiable = Identifiable
module In_channel = In_channel
module Int63 = Core_int63
module Int_intf = Int_intf
module Int_set = Int_set
module Interfaces = Interfaces
module Interval = Interval
module Field = Core_field
module Linebuf = Linebuf
module Lock_file = Lock_file
module Memo = Memo
module Monad = Monad
module Month = Month
module Mutex = Core_mutex
module Ofday = Ofday
module Option = Option
module Only_in_test = Utest.Only_in_test
module Out_channel = Out_channel
module Piecewise_linear = Piecewise_linear
module Pretty_printer = Pretty_printer
module Printexc = Core_printexc
module Quickcheck = Quickcheck
module Result = Result
module Robustly_comparable = Robustly_comparable
module Set_once = Set_once
module Sexpable = Sexpable
module Sexp_maybe = Core_sexp.Sexp_maybe
module Signal = Signal
module Space_safe_tuple2 = Space_safe_tuple.T2
module Space_safe_tuple3 = Space_safe_tuple.T3
module Span = Span
module Squeue = Squeue
module Stringable = Stringable
module String_id = String_id
module Substring = Substring
module Substring_intf = Substring_intf
module Thread = Core_thread
module Thread_safe_queue = Thread_safe_queue
module Timer = Timer
module Tuple = Tuple
module Tuple2 = Tuple.T2
module Tuple3 = Tuple.T3
module TZ = TZ
module Unique_id = Unique_id
module Unique_id_intf = Unique_id_intf
module Unit = Unit
module Unix = Core_unix
module Utest = Utest
module Weekday = Weekday
module Word_size = Word_size

module type Unique_id = Unique_id_intf.Id
