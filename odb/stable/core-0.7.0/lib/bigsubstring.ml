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

include Make_substring.F (struct
  type t = Bigstring.t

  let create = Bigstring.create
  let length = Bigstring.length
  module Blit = Make_substring.Blit
  let blit = Blit.bigstring_bigstring
  let blit_to_string = Blit.bigstring_string
  let blit_to_bigstring = Blit.bigstring_bigstring
  let blit_from_string = Blit.string_bigstring
  let blit_from_bigstring = Blit.bigstring_bigstring
  let of_bigstring t = t
  let of_string s = Bigstring.of_string s
end)
