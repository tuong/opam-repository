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

open Core.Std

let default_callback ~elapsed =
  eprintf "tick thread stopped for %s\n%!" (Time.Span.to_string elapsed)
;;

let start ?(callback = default_callback) () =
  let r = ref (Time.now ()) in
  
  ignore
    (Thread.create (fun () ->
      while true do
        Time.pause (Time.Span.of_ms 10.0);
        let now = Time.now () in
        let elapsed = Time.diff now !r in
        r := now;
        if Time.Span.(>) elapsed (Time.Span.of_ms 50.) then callback ~elapsed;
      done)
       () : Thread.t);
;;
