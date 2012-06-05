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

open Unix
open OUnit;;
open Core.Std
open Bigstring_test

let io_test ~n =
  fdpair_test ~n socketpair
    (fun buf fd ->
      let oc = out_channel_of_descr fd in
      Bigbuffer.output_buffer oc buf;
      flush oc)
    (fun ~n:_ orig_buf fd ->
      let ic = in_channel_of_descr fd in
      let buf = Bigbuffer.create 0 in
      Bigbuffer.add_channel buf ic (Bigbuffer.length orig_buf);
      "channel" @? (Bigbuffer.contents orig_buf = Bigbuffer.contents buf))

let test =
  "Bigbuffer" >:::
    [
      "adding/extracting data" >::
        (fun () ->
          let buf = Bigbuffer.create 100 in
          Bigbuffer.add_char buf 'x';
          Bigbuffer.add_char buf 'y';
          Bigbuffer.add_string buf "asdf";
          Bigbuffer.add_substring buf "fdsa" 1 2;
          Bigbuffer.add_buffer buf buf;
          let str = "xyasdfds" in
          "contents" @? (Bigbuffer.contents buf = str ^ str);
          "big_contents" @?
            (Bigstring.to_string (Bigbuffer.big_contents buf) = str ^ str );
          "sub" @? (Bigbuffer.sub buf 5 5 = "fdsxy");
          io_test ~n:"" buf
        );

    ]
