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


open OUnit;;
open Core.Std

module StringMap = Map.Make (String)

let m1 = StringMap.of_alist_exn ["a",1; "b",2; "c",3; "d",4]
let m2 = StringMap.of_alist_exn ["a",1; "c",-3; "d",4; "e",5]

let test = 
  "core_fmap" >:::
    [
      "merge1" >::
        (fun () ->
           let f ~key:_ d1 d2 = match d1,d2 with
             | None,_ | _, None -> None
             | Some x, Some y -> Some (x+y)
           in
           "eq1" @? StringMap.equal (=) (StringMap.merge ~f m1 m2)
             (StringMap.of_alist_exn ["a",2;"c",0;"d",8;]);
           "eq2" @? StringMap.equal (=) (StringMap.merge ~f m2 m1)
             (StringMap.of_alist_exn ["a",2;"c",0;"d",8;]);
        );
      "merge2" >::
        (fun () ->
           let f ~key:_ d1 d2 = match d1,d2 with
             | None, None -> None
             | Some x, None -> Some x
             | None, Some _ -> None
             | Some x, Some y -> Some (x+y)
           in
           "eq" @? StringMap.equal (=) (StringMap.merge ~f m1 m2)
             (StringMap.of_alist_exn ["a",2;"b",2;"c",0;"d",8;])
        );
      "merge3" >::
        (fun () ->
           let f ~key:_ d1 d2 = match d1,d2 with
             | None,None -> None
             | Some x, None | None, Some x -> Some x
             | Some x, Some y -> Some (x+y)
           in
           "eq1" @? StringMap.equal (=) (StringMap.merge ~f m1 m2)
             (StringMap.of_alist_exn ["a",2;"b",2;"c",0;"d",8;"e",5]);
           "eq2" @? StringMap.equal (=) (StringMap.merge ~f m2 m1)
             (StringMap.of_alist_exn ["a",2;"b",2;"c",0;"d",8;"e",5])
        );
      "merge3" >::
        (fun () ->
           let f ~key:_ d1 d2 = match d1,d2 with
             | None,None -> None
             | Some x, None | None, Some x -> Some x
             | Some x, Some y -> Some (x+y)
           in
           "eq1" @? StringMap.equal (=) (StringMap.merge ~f m1 StringMap.empty) m1;
           "eq2" @? StringMap.equal (=) (StringMap.merge ~f StringMap.empty m1) m1;
        );
      "sexp" >::
        (fun () ->
          let s = "((a 1) (b 2) (c 3) (d 4))" in
          let m1' = StringMap.t_of_sexp int_of_sexp (Sexp.of_string s) in
          "of_sexp1" @? (StringMap.equal (=) m1' m1);
          let s_dup = "((a 1) (b 2) (a 3) (d 4))" in
          let s_dup = Sexp.of_string s_dup in
          assert_raises
            (Sexplib.Conv.Of_sexp_error (
              Failure "Map.t_of_sexp: duplicate key", sexp_of_string "a"))
            (fun () -> StringMap.t_of_sexp int_of_sexp s_dup)
        );
      "of_alist" >::
        (fun () -> 
          let a = [("a", 1); ("b", 2); ("c", 3); ("d", 4)] in
          let m = 
            match StringMap.of_alist a with `Ok x -> x | `Duplicate_key _ -> failwith "argh"
          in
          "1" @? (StringMap.find_exn m "a" = 1 && StringMap.find_exn m "d" = 4);
          let a_dup = [("a", 1); ("b", 2); ("c", 3);  ("b", 4);  ("e", 5)] in
          "2" @? 
            (match StringMap.of_alist a_dup with `Ok _ -> false | `Duplicate_key x -> x = "b");
          "3" @?
            ((List.sort ~cmp:ascending
                (StringMap.to_alist (StringMap.of_alist_exn a)))
              = List.sort ~cmp:ascending a);
          assert_raises
            (Failure "Map.of_alist_exn: duplicate key")
            (fun () -> StringMap.of_alist_exn a_dup);
        );
      "for_all/exists" >:: (fun () ->
        let m = StringMap.of_alist_exn ["a",1;"b",2;"c",3;"d",4] in
        "1" @? (StringMap.for_all ~f:(fun x -> x > 0) m);
        "2" @? (not (StringMap.for_all ~f:(fun x -> x % 2 = 0) m));
        "3" @? (StringMap.exists ~f:(fun x -> x % 2 = 0) m);
        "4" @? (not (StringMap.exists ~f:(fun x -> x < 0) m));
        "short circuit forall" @? (
          let sum = ref 0 in
          ignore (StringMap.for_all m ~f:(fun x -> sum := !sum + x; x <> 1));
          !sum = 1
        );
        "short circuit exists" @? (
          let sum = ref 0 in
          ignore (StringMap.exists m ~f:(fun x -> sum := !sum + x; x = 1));
          !sum = 1
        );
      );
    ]
