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

module Extended_date : sig
  (**
     Shortcut for calling Core_extended.Unix.strftime without having to
     create a Time.t and convert it to a Unix.tm.

     [format "%Y-%m-%d" t] will return "YYYY-MM-DD"
  *)
  val format : ?ofday:Time.Ofday.t -> string -> Date.t -> string
end

module Extended_span : sig
  (**
     Convert a time span to a human-readable string, e.g. "1:23:45.778"
     (versus "1.396h" from [Time.Span.to_string]).
  *)
  val to_string_hum : Time.Span.t -> string
end
