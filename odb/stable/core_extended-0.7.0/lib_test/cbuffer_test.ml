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

open OUnit;;
open Core_extended
open Std

let ident x = x

let collect list obj = list := obj::!list

let test = "cbuffer" >::: [
  "mapping" >:: (fun () ->
    let cb = Cbuffer.of_array [|0;1;2;3;4|] in
    let bu = Buffer.create 10 in
    let out n x = Buffer.add_string bu (Printf.sprintf "(%d %d)" n x) in
    Cbuffer.iter cb ~f:out;
    "map" @? (Buffer.contents bu = "(0 0)(1 1)(2 2)(3 3)(4 4)");
    Buffer.clear bu;
    Cbuffer.iterr cb ~f:out;
    "mapr" @? (Buffer.contents bu = "(4 4)(3 3)(2 2)(1 1)(0 0)"));
  "compare" >:: (fun () ->
    let cb1 = Cbuffer.of_array [|0;1;2;3;4|] in
    let cb2 = Cbuffer.of_array [|0;1;2;2;2|] in
    "yes" @? (Cbuffer.cb_compare ~f:(=) ~b1:cb1 ~b2:cb2 ~s1:0 ~s2:0 ~n:3);
    "no1" @? not (Cbuffer.cb_compare ~f:(=) ~b1:cb1 ~b2:cb2 ~s1:0 ~s2:0 ~n:4);
    "no2" @? not (Cbuffer.cb_compare ~f:(=) ~b1:cb1 ~b2:cb2 ~s1:1 ~s2:0 ~n:3));
  "drop_old" >:: (fun () ->
    let cb = Cbuffer.of_array [|4;3;2;1;0|] in
    let list = ref [] in
    let free = collect list in
    "ret-1" @? (Cbuffer.drop_old ~cutoff:(-1) ~free cb ~f:ident = 0);
    "buf-1" @? (Cbuffer.to_array cb = [|4;3;2;1;0|]);
    "free-1" @? (!list = []);
    "ret0" @? (Cbuffer.drop_old ~cutoff:0 ~free cb ~f:ident = 1);
    "buf0" @? (Cbuffer.to_array cb = [|4;3;2;1|]);
    "free0" @? (!list = [0]);
    "ret1" @? (Cbuffer.drop_old ~cutoff:1 ~free cb ~f:ident = 1);
    "buf1" @? (Cbuffer.to_array cb = [|4;3;2|]);
    "free1" @? (!list = [1;0]);
    "ret2" @? (Cbuffer.drop_old ~cutoff:2 ~free cb ~f:ident = 1);
    "buf2" @? (Cbuffer.to_array cb = [|4;3|]);
    "free2" @? (!list = [2;1;0]);
    "ret3" @? (Cbuffer.drop_old ~cutoff:3 ~free cb ~f:ident = 1);
    "buf3" @? (Cbuffer.to_array cb = [|4|]);
    "free3" @? (!list = [3;2;1;0]);
    "ret4" @? (Cbuffer.drop_old ~cutoff:4 ~free cb ~f:ident = 1);
    "buf4" @? (Cbuffer.to_array cb = [||]);
    "free4" @? (!list = [4;3;2;1;0]);
    let cb = Cbuffer.of_array [|4;3;2;1;0|] in
    "ret5" @? (Cbuffer.drop_old ~cutoff:5 cb ~f:ident = 5);
    "buf5" @? (Cbuffer.to_array cb = [||]);
  );
]

