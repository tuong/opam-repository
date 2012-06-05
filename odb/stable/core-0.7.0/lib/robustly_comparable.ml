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

(* This interface compares float-like objects with a small tolerance.  For example [>.]
   returns false if the floats are almost equal, and [=.] returns true if the floats are
   almost but not quite equal.  The tolerance is intended to be about right for
   human-entered values like prices and seconds. *)

module type S = sig
  type robustly_comparable
  val ( >=. ) : robustly_comparable -> robustly_comparable -> bool
  val ( <=. ) : robustly_comparable -> robustly_comparable -> bool
  val ( =. ) : robustly_comparable -> robustly_comparable -> bool
  val ( >. ) : robustly_comparable -> robustly_comparable -> bool
  val ( <. ) : robustly_comparable -> robustly_comparable -> bool
  val ( <>. ) : robustly_comparable -> robustly_comparable -> bool
end
