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
open Core_extended.Std
open OUnit;;
let test =
  "extended_string.Escaping" >:::
    [ "escape_gen" >::
        (fun () ->
          let escape = String.Escaping.escape_gen
            ~escapeworthy_map:[('%','p');('^','c')] ~escape_char:'_'
          in
          "nothing" @? ((escape "foo") = "foo");
          "foo" @? (escape "_" = "__");
          "bar" @? (escape "foo%bar" = "foo_pbar");
          "baz" @? (escape "^foo%" = "_cfoo_p");
        );
      "escape" >::
        (fun () ->
          let escape = String.Escaping.escape
            ~escapeworthy:['%';'^'] ~escape_char:'_' in
          "nothing" @? (escape "foo" = "foo");
          "foo" @? (escape "_" = "__");
          "bar" @? (escape "foo%bar" = "foo_%bar");
          "baz" @? (escape "^foo%" = "_^foo_%");
        );
      "escape_one_orig" >::
        (fun () ->
          let escape = String.Escaping.escape_one_orig
            ~escapeworthy:'%' ~escape_char:'_' in
          "nothing" @? (escape "foo" = "foo");
          "foo" @? (escape "_" = "__");
          "bar" @? (escape "foo%bar" = "foo_%bar");
          "baz" @? (escape "%foo%" = "_%foo_%");
        );
      "escape_two_orig" >::
        (fun () ->
          let escape = String.Escaping.escape_two_orig
            ~escapeworthy1:'%' ~escapeworthy2:'^' ~escape_char:'_'
          in
          "nothing" @? (escape "foo" = "foo");
          "foo" @? (escape "_" = "__");
          "bar" @? (escape "foo%bar" = "foo_%bar");
          "baz" @? (escape "^foo%" = "_^foo_%");
        );
      "unescape_gen" >::
        (fun () ->
          let unescape = String.Escaping.unescape_gen
            ~map:['p','%';'c','^'] ~escape_char:'_' in
          "nothing" @? (unescape "foo" = "foo");
          "foo" @? (unescape "__" = "_");
          "bar" @? (unescape "foo_pbar" = "foo%bar");
          "baz" @? (unescape "_cfoo_p" = "^foo%");
        );
      "unescape" >::
        (fun () ->
          let unescape = String.Escaping.unescape ~escape_char:'_' in
          "nothing" @? (unescape "foo" = "foo");
          "foo" @? (unescape "__" = "_");
          "bar" @? (unescape "foo_%bar" = "foo%bar");
          "baz" @? (unescape "_^foo_%" = "^foo%");
        );
    ]
