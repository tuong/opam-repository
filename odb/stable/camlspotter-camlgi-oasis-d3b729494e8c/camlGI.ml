(* File: camlGI.ml

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
(* 	$Id: camlGI.ml,v 1.1 2005/06/12 21:45:11 chris_77 Exp $	 *)

(* This file only gathers the various modules, much as -pack would do
   but without its limitations (i.e., works on all platforms). *)

module Cgi_types = struct INCLUDE "cgi_types.ml" end

(* module type CGI_COMMON *)
INCLUDE "cgi_common.mli"
module Cgi_common : CGI_COMMON = struct INCLUDE "cgi_common.ml" end


module Expires = struct INCLUDE "expires.ml" end
module Cookie = struct INCLUDE "cookie.ml" end
module Sendmail = struct INCLUDE "sendmail.ml" end

module Cgi_std  = struct INCLUDE "cgi_std.ml" end
module Cgi_fast = struct INCLUDE "cgi_fast.ml" end
module Cgi = struct INCLUDE "cgi.ml" end

module Template = struct INCLUDE "template.ml" end
module DbiPool  = struct INCLUDE "dbiPool.ml" end
