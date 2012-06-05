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

open Core.Std
open OUnit
open Unix

let with_large_file f =
  let filename = Filename.temp_file "large" "test" in
  protect ~f:(fun () ->
    ignore (system (sprintf
      "dd if=/dev/zero of=%s bs=1 count=1 seek=$(echo '2 ^ 32 + 1' | bc -l) > /dev/null 2>/dev/null"
        filename));
    f filename)
    ~finally:(fun () -> Sys.remove filename);
  true

let test =
  "core_unix" >:::
    [ "stat" >::
      (fun () ->
        "stat-large" @? with_large_file (fun fn -> stat fn);
        "lstat-large" @? with_large_file (fun fn -> lstat fn));
    ]
