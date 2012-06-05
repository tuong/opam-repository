(* File: cgi_common.mli

   Objective Caml Library for writing (F)CGI programs.

   Copyright (C) 2004

     Christophe Troestler
     email: Christophe.Troestler@umh.ac.be
     WWW: http://www.umh.ac.be/math/an/software/

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation, with the
   special exception on linking described in file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*)
(* 	$Id: cgi_common.mli,v 1.6 2005/06/12 21:37:20 chris_77 Exp $	 *)

(** Internal library common to CGI and FCGI. *)
module type CGI_COMMON = sig

(** {2 Dealing with query strings} *)

val rev_split : char -> string -> string list
  (** [split_rev delim s] split [s] at each character [delim] and
      returns the list of substrings in reverse order.  The substrings
      do not share [s] memory. *)

val decode_range : string -> int -> int -> string
  (** [decode_range s low up] decodes the substring [s.[low .. up-1]]
      i.e., in the returned string the '%XX' and '+' are converted
      into their chars.  The range [s.[low .. up-1]] is overwritten.
      It returns the decoded string with heading and trailing spaces
      stripped.

      It is ASSUMED that the range is valid i.e., [0 <= low] and [up <=
      String.length s].  Invalid '%XX' are left unchanged. *)

val encode : string -> string
  (** [encode s] encodes [s] according to URI specs. *)

val encode_cookie : string -> string
  (** [encode_cookie s] encodes special characters according to
      RFC2068.  The mean of encoding is the same as for [encode] (not
      specified in RFC2068), so only one decoding function is needed.
  *)

val char_of_hex : int -> char
  (** [char_of_hex i] returns the char corresponding to [i] which is
      ASSUMED to be one of the values [0 .. 15].  *)

val parse_query_range : string -> int -> int -> (string, string) Hashtbl.t
   -> unit
  (** [parse_query_range qs low up h] parses [s.[low .. up-1]],
      extracts the (decoded) key-val pairs and add them to [h].  The
      substring [s.[low .. up-1]] is overwritten. *)

val parse_query : string -> (string, string) Hashtbl.t -> unit
  (** [parse_query qs h] parses [s], extracts the (decoded) key-val
      pairs and add them to [h].  The string [s] is overwritten. *)


(** {2 Multipart data} *)

exception Unsupported_media_type of string

val parse_post_data : string -> Cgi_types.request -> unit
  (** [parse_post_data data request] parse [data]
      according to [content_type] and add the resulting parameters and
      uploads to the [request].

      @raise Unsupported_media_type if the [content_type] is not
      supported or if the [data] does not conform the
      [content_type]. *)


(** {2 Dates} *)

val string_of_weekday : int -> string
  (** [string_of_weekday d] converts a number [d] ASSUMED to be one in
      0 .. 7 into a 3 letters weekday (0 and 7 being both Sunday). *)
val string_of_month : int -> string
  (** [string_of_month m] converts a number [m] ASSUMED to be one in 0
      .. 11 into a 3 letters month abbreviation (0 -> Jan,...). *)


(** {2 Default error template} *)

val error_html : int -> string -> string -> string
(** [error_html errn msg email] returns an HTML text informing the
    user of the error number [errn] with message [msg], telling him
    that he can contact the webmaster at the address [email]. *)

end
