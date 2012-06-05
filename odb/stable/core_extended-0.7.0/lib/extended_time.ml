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

module Extended_date = struct
  let format ?(ofday=Time.Ofday.start_of_day) s t = 
    Time.format (Time.of_date_ofday t ofday) s
end

module Extended_span = struct
  let to_string_hum (t : Time.Span.t) =
    let sign_str = 
      match Float.sign (t :> float) with
      | Float.Sign.Neg -> "-"
      | Float.Sign.Zero | Float.Sign.Pos -> ""
    in
    let rest = 
      match Float.classify (t :> float) with
      | Float.Class.Subnormal | Float.Class.Zero -> "0:00:00.000"
      | Float.Class.Infinite -> "inf"
      | Float.Class.Nan -> "nan"
      | Float.Class.Normal ->
          let parts = Time.Span.to_parts t in
          let module P = Time.Span.Parts in
          sprintf "%d:%02d:%02d.%03d" parts.P.hr parts.P.min parts.P.sec parts.P.ms
    in
    sign_str ^ rest
end
