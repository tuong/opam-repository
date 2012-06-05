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

type 'a reader = 'a Read_ml.reader
type ('a, 'b) reader1 = 'a Unsafe_read_c.reader -> 'b Read_ml.reader
type ('a, 'b, 'c) reader2 = 'a Unsafe_read_c.reader -> ('b, 'c) reader1
type ('a, 'b, 'c, 'd) reader3 = 'a Unsafe_read_c.reader -> ('b, 'c, 'd) reader2

type 'a writer = 'a Write_ml.writer
type ('a, 'b) writer1 = 'a Unsafe_write_c.writer -> 'b Write_ml.writer
type ('a, 'b, 'c) writer2 = 'a Unsafe_write_c.writer -> ('b, 'c) writer1
type ('a, 'b, 'c, 'd) writer3 = 'a Unsafe_write_c.writer -> ('b, 'c, 'd) writer2
