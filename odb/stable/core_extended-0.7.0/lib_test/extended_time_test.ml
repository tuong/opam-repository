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

open OUnit
open Core.Std
module Time = Core_extended.Std.Time

let test =
  "extended_time.Extended_span" >:::
    [ "to_string_hum" >::
        (fun () ->
          let t secs str =
            (string_of_float secs) @?
            (Time.Extended_span.to_string_hum (Time.Span.of_sec secs) = str)
          in
          t 0. "0:00:00.000";
          t 0.075 "0:00:00.075";
          t 3.075 "0:00:03.075";
          t 163.075 "0:02:43.075";
          t 3763.075 "1:02:43.075";
          t 432163.075 "120:02:43.075";
          t (-. 432163.075) "-120:02:43.075";
        );
    ]
