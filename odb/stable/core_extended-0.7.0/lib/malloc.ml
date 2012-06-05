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

(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
open Sexplib.Wrapper
open Bin_prot.Std

type mallinfo = {
  arena : int;
  ordblks : int;
  smblks : int;
  hblks : int;
  hblkhd : int;
  usmblks : int;
  fsmblks : int;
  uordblks : int;
  fordblks : int;
  keepcost : int;
} with sexp, bin_io

external mallinfo : unit -> mallinfo = "malloc_mallinfo_stub"

type opt =
  | TRIM_THRESHOLD
  | TOP_PAD
  | MMAP_THRESHOLD
  | MMAP_MAX
  | CHECK_ACTION
(*   | PERTURB *)
with sexp, bin_io

external mallopt : opt -> int -> unit = "malloc_mallopt_stub"

external malloc_trim : int -> unit = "malloc_trim_stub"

external malloc_stats : unit -> unit = "malloc_stats_stub"
