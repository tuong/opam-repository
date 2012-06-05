(***********************************************************************)
(*                                                                     *)
(*                              My_stdlib                              *)
(*                                                                     *)
(*             projets Estime & Cristal, INRIA-Rocquencourt            *)
(*                                                                     *)
(*  Copyright 2004-2007 INRIA.                                         *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved. This file is distributed only by permission.  *)
(*                                                                     *)
(*  Francois Clement <Francois.Clement@inria.fr>                       *)
(*  Pierre Weis <Pierre.Weis@inria.fr>                                 *)
(*                                                                     *)
(***********************************************************************)

(* $Id: ubuffer.ml,v 1.1 2007-04-02 17:48:29 weis Exp $ *)

(* Flushing buffers on channels *)

let flush_buffer oc ob =
  Buffer.output_buffer oc ob;
  Buffer.clear ob;
  flush oc;;

let flush_buffer_on_stdout = flush_buffer stdout;;

let flush_buffer_on_file fname ob =
  let oc = open_out fname in
  flush_buffer oc ob;
  close_out oc;;
