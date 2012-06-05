(******************************************************************************
 *                             Bin-prot                                       *
 *                                                                            *
 * Copyright (C) 2008- Jane Street Holding, LLC                               *
 *    Contact: opensource@janestreet.com                                      *
 *    WWW: http://www.janestreet.com/ocaml                                    *
 *    Author: Markus Mottl                                                    *
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

open Bin_prot.Std
module type S = sig end

include (struct
  type t = int
  with bin_io
end : S)

include (struct
  type t = int32
  with bin_io
end : S)

include (struct
  type t = int64
  with bin_io
end : S)

include (struct
  type t = nativeint
  with bin_io
end : S)

include (struct
  type t = float
  with bin_io
end : S)

include (struct
  type t = char
  with bin_io
end : S)

include (struct
  type t = int list
  with bin_io
end : S)

include (struct
  type t = float array
  with bin_io
end : S)

include (struct
  type t = int64 array
  with bin_io
end : S)

include (struct
  type t = int * float * char
  with bin_io
end : S)

include (struct
  type t = A | B
  with bin_io

  type u = C | D | E of t
  with bin_io
end : S)

include (struct
  type t = [ `A | `B ]
  with bin_io

  type u = [ `C | `D | `E of t ]
  with bin_io
end : S)

include (struct
  type a = [ `A1 | `A2 ]
  with bin_io

  type b = [ `B1 | `B2 ]
  with bin_io

  type t = [ a | b ]
  with bin_io
end : S)

include (struct
  type t = {
    foo : char;
    bar : int;
    baz : string;
  } with bin_io
end : S)

include (struct
  type 'a t = 'a
  with bin_io
end : S)

include (struct
  type 'a t = 'a * int
  with bin_io
end : S)

include (struct
  type ('a, 'b) t = 'a * 'b
  with bin_io
end : S)

include (struct
  type 'a t = 'a constraint 'a = [< `A | `B ]
  with bin_io

  type 'a u = [`A] t
  with bin_io
end : S)

include (struct
  type 'a t = {
    foo : 'a;
    bar : int;
  } with bin_io
end : S)
