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

(* The Caml module binds everything that is available in the standard
   environment so that we can easily refer to standard things even if they've
   been rebound.
*)

module Arg = Arg
module Array = Array
module ArrayLabels = ArrayLabels
module Buffer = Buffer
module Callback = Callback
module Char = Char
module Complex = Complex
module Condition = Condition
module Digest = Digest
module Event = Event
module Filename = Filename
module Format = Format
module Gc = Gc
module Genlex = Genlex
module Hashtbl = Hashtbl
module Int32 = Int32
module Int64 = Int64
module Lazy = Lazy
module Lexing = Lexing
module List = List
module ListLabels = ListLabels
module Map = Map
module Marshal = Marshal
module MoreLabels = MoreLabels
module Mutex = Mutex
module Nativeint = Nativeint
module Oo = Oo
module Parsing = Parsing
module Pervasives = Pervasives
module Printexc = Printexc
module Printf = Printf
module Queue = Queue
module Random = Random
module Scanf = Scanf
module Set = Set
module Sort = Sort
module Stack = Stack
module StdLabels = StdLabels
module Stream = Stream
module String = String
module StringLabels = StringLabels
module Sys = Sys
module Thread = Thread
module Unix = Unix
module UnixLabels = UnixLabels
module Weak = Weak

include Pervasives
