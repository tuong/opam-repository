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

open Core.Std
open OUnit;;

module Date = Date
module Ofday = Time.Ofday
module Span = Time.Span

let teq t1 t2 =
  if Sys.word_size = 64 then
    round_towards_zero (Time.to_float t1 *. 1000.) = round_towards_zero (Time.to_float t2 *. 1000.)
  else
    true (* milliseconds since 1970 too large for truncate *)
let speq s1 s2 = round (Span.to_ms s1) = round (Span.to_ms s2)
let convtest ?tol f1 f2 =
  let x = float (Random.int 1_000_000) /. 1000. in
  let tol = match tol with
    | None -> Float.epsilon
    | Some pct -> x *. pct
  in
  abs_float (f1 (f2 x) -. x) <= tol

let mintime_str = "0000-01-01 00:00:00.000000"
let maxtime_str = "3000-01-01 00:00:00.000000"

let time_gen () = Time.of_float (Quickcheck.fg ())

let reasonable_time time =              (* between about 1970 and 2070 *)
  let time = Time.to_float time in
  time > 0. && time < 100. *. 52. *. 24. *. 60. *. 60.

let similar_time time time' =
  let time = Time.to_float time in
  let time' = Time.to_float time' in
  abs_float (time -. time') < 0.01

let test_list = ref []
let add name test = test_list := (name >:: test) :: !test_list

let () = add "t"
  (fun () ->
      let s1 = "2005-05-25 12:46" in
      let s2 = "2005-05-25 12:46:15" in
      let s3 = "2005-05-25 12:46:15.232" in
      let time1 = Time.of_string s1 in
      let time2 = Time.of_string s2 in
      let time3 = Time.of_string s3 in
      let now1 = Time.now () in
      let now2 = Time.now () in
      "diff1" @? (Float.iround_exn (Span.to_sec (Time.diff time2 time1)) = 15);
      "diff1'" @? (Float.iround_exn (Span.to_ms (Time.diff time2 time1)) = 15 * 1000);
      "diff2" @? (Float.iround_exn (Span.to_ms (Time.diff time3 time2)) = 232);
      "conv1" @? (Time.to_string time1 = s1 ^ ":00.000000");
      "conv2" @? (Time.to_string time2 = s2 ^ ".000000");
      "conv3" @? (Time.to_string time3 = s3 ^ "000");
      "ord" @? (now2 >= now1);
      "sexp1" @? (Time.t_of_sexp (Time.sexp_of_t time1) = time1);
      "sexp2" @? (Time.t_of_sexp (Time.sexp_of_t time2) = time2);
      "sexp3" @? (Time.t_of_sexp (Time.sexp_of_t time3) = time3);
      let date, ofday = Time.to_date_ofday time3 in
      "date" @? (date = Date.of_string "2005-05-25");
      "ofday" @? (Time.Ofday.to_sec ofday =.
          Time.Ofday.to_sec (Time.Ofday.of_string "12:46:15.232"));
      "ofday1" @? (Time.Ofday.of_string "09:13" = Time.Ofday.of_string "0913");
      "add1" @? teq (Time.add time1 (Span.of_sec 15.)) time2;
      "add2" @? teq (Time.add time2 (Span.of_ms 232.)) time3;
    )

let () =
  add "Ofday_string_conversion"
    (fun () ->
      (* We want to test a number of times during the day, but testing all
         possible times is too expensive.  We also want the test to always be
         the same, so this uses a specific Random.State to generate a repeatable
         series of random times to test *)
      let rand_state = Random.State.make [| 1; 2; 3; 4; 5; 6; 7 |] in
      for i = 0 to 100_000 do
        let secs = Random.State.int rand_state 86_400_000 in
        let ofday = Ofday.of_span_since_midnight (Span.of_ms (float secs)) in
        let ofday_string = Ofday.to_string ofday in
        let ofday' = Ofday.of_string ofday_string in
        if Ofday.(<>) ofday ofday' then
          failwithf "(%d seconds) %s (%f) <> Ofday.of_string %s (%f)"
            secs ofday_string (Ofday.to_float ofday) (Ofday.to_string ofday')
            (Ofday.to_float ofday') ();
        let ofday' = Ofday.of_string_iso8601_extended ofday_string in
        if Ofday.(<>) ofday ofday' then
          failwithf "%s <> Ofday.of_string_iso8601_extended %s"
            ofday_string (Ofday.to_string ofday') ();
      done)


let () =
  add "date"
    (fun () ->
      let start =
        Time.of_date_ofday
          (Date.create ~y:1999 ~m:Month.jan ~d:1)
          Ofday.start_of_day
      in
      let day = Span.of_day 1. in
      for i = 0 to 100_000 do
        let date =
          Time.to_date (Time.add start (Span.scale day (float i)))
        in
        let date_string = Date.to_string date in
        let date' = Date.of_string date_string in
        if Date.(<>) date date' then
          failwithf "%s <> Date.of_string %s"
            date_string (Date.to_string date') ();
      done)

let () =
  add "add_days"
    (fun () ->
      "one" @? (Date.add_days (Date.of_string "2008-11-02") 1 =
        Date.of_string "2008-11-03");
      "two" @? (Date.add_days (Date.of_string "2008-11-02") 2 =
        Date.of_string "2008-11-04");
      "leap" @? (Date.add_days (Date.of_string "2000-02-28") 1 =
        Date.of_string "2000-02-29")
    )

let () =
  add "add_months"
    (fun () ->
      "zero" @? (Date.add_months (Date.of_string "2009-02-28") 0 =
        Date.of_string "2009-02-28");
      "one" @? (Date.add_months (Date.of_string "2009-01-30") 1 =
        Date.of_string "2009-02-28");
      "two" @? (Date.add_months (Date.of_string "2009-01-30") 2 =
        Date.of_string "2009-03-30");
      "december" @? (Date.add_months (Date.of_string "2009-02-28") 10 =
        Date.of_string "2009-12-28");
      "neg" @? (Date.add_months (Date.of_string "2009-01-30") (-11) =
        Date.of_string "2008-02-29")
    )

let () =
  add "add_weekdays"
    (fun () ->
      let test lbl d1 n d2 =
        lbl @? (Date.add_weekdays
        (Date.of_string d1) n = Date.of_string d2)
      in
      test "one" "2009-01-01" 1 "2009-01-02";
      test "one_weekend" "2009-01-02" 1 "2009-01-05";
      test "neg_one" "2009-01-02" (-1) "2009-01-01";
      test "neg_one_weekend" "2009-01-05" (-1) "2009-01-02";
      test "neg_two_weekend" "2009-01-06" (-2) "2009-01-02";
      test "non_leap_weekend" "2009-02-27" 1 "2009-03-02";
      test "leap_weekend" "2008-02-28" 2 "2008-03-03";
    )
;;

let () =
  add "add_business_days"
    (fun () ->
      let test lbl d1 n d2 =
        let is_holiday d =
          List.mem d ~set:(List.map [
            "2009-01-01";
            "2009-03-01";
            "2009-03-02";
          ] ~f:Date.of_string)
        in
        let res = Date.add_business_days ~is_holiday (Date.of_string d1) n in
        lbl @? (res = Date.of_string d2)
      in
      test "one" "2009-01-01" 1 "2009-01-05";
      test "one_weekend" "2009-01-02" 1 "2009-01-05";
      test "neg_one" "2009-01-02" (-1) "2008-12-31";
      test "neg_one_weekend" "2009-01-05" (-1) "2009-01-02";
      test "neg_two_weekend" "2009-01-06" (-2) "2009-01-02";
      test "non_leap_weekend" "2009-02-27" 1 "2009-03-03";
      test "leap_weekend" "2008-02-28" 2 "2008-03-03";
    )
;;

let () =
  add "span_scale"
    (fun () ->
      "ms" @? speq (Span.scale (Span.of_sec 10.) 0.001) (Span.of_ms 10.);
      "min" @? speq (Span.scale (Span.of_sec 10.) 60.) (Span.of_min 10.);
      "hr" @? speq (Span.scale (Span.of_sec 10.) (60. *. 60.)) (Span.of_hr 10.);
    );
  add "span_conv"
    (fun () ->
      for i = 1 to 100 do
        "sec" @? convtest Span.to_sec Span.of_sec;
        "ms" @? convtest Span.to_ms Span.of_ms;
        "min" @? convtest (fun x -> Span.to_sec x /. 60.) Span.of_min;
        "hr" @? convtest (fun x -> Span.to_sec x /. 60. /. 60.) Span.of_hr;
        "sexp" @?
          convtest ~tol:0.0001
          (fun x -> Span.to_sec (Span.t_of_sexp x))
          (fun x -> Span.sexp_of_t (Span.of_sec x));
      done
    );
  add "date"
    (fun () ->
      let d = Date.create ~y:2004 ~m:Month.apr ~d:15 in
      "conv1" @? (Date.to_string d = "2004-04-15");
      "conv2" @? (d = Date.of_string "2004-04-15");
      "conv3" @? (d = Date.of_string "20040415");
      "conv4" @? (d = Date.of_string "15APR2004");
      "conv5" @? (d = Date.of_string "04/15/2004");
      "conv6" @? (d = Date.of_string "2004/04/15");
      "conv7" @? (d = Date.of_string "4/15/4");
    );
  add "norollover"
    (fun () ->
      let t1 = Time.of_string "2005-05-25 12:46:59.900" in
      let t2 = Time.add t1 (Span.of_ms 99.9) in
      "60secspr" @? ((Time.to_string t2) = "2005-05-25 12:46:59.999900");
    );
  add "to_string,of_string"
    (fun () ->
      let check time =
        if reasonable_time time
        then
          let time' = Time.of_string (Time.to_string time) in
          similar_time time time'
        else true
      in
      Quickcheck.laws_exn "string" 100 time_gen check;
    );
  add "to_string,of_string2"
    (fun () ->
      let s = "2005-06-01 10:15:08.047123" in
      let t = Time.of_string s in
      "foo" @? (Time.to_string t = s)
    );
  add "to_string,of_string3"
    (fun () ->
      let s = "2006-06-16 04:37:07.082945" in
      let t = Time.of_string s in
      "foo" @? (Time.to_string t = s)
    );
  add "to_filename_string,of_filename_string"
    (fun () ->
      let check time =
        if reasonable_time time
        then
          let time' = Time.of_filename_string (Time.to_filename_string time) in
          similar_time time time'
        else true
      in
      Quickcheck.laws_exn "string" 100 time_gen check;
    );
  add "to_filename_string,of_filename_string2"
    (fun () ->
      let s = "2005-06-01_10-15-08.047983" in
      let t = Time.of_filename_string s in
      "foo" @? (Time.to_filename_string t = s)
    );
  add "of_sexp,to_sexp"
    (fun () ->
      let check time =
        if reasonable_time time
        then
          let time' = Time.t_of_sexp (Time.sexp_of_t time) in
          similar_time time time'
        else true
      in
      Quickcheck.laws_exn "sexp" 100 time_gen check;
    );
  add "daylight_saving_time"
    (fun () ->
      let s = "2006-04-02 23:00:00.000000" in
      let time = Time.of_string s in
      "dst" @? (Time.to_string time = s)
    );
  add "weird_date_in_time"
    (fun () ->
      let t1 = Time.of_string "01 JAN 2008 10:37:22.551" in
      "rnse1" @? (Time.to_string t1 = "2008-01-01 10:37:22.551000");
      let t2 = Time.of_string "01 FEB 2008 17:38:44.031" in
      "rnse2" @? (Time.to_string t2 = "2008-02-01 17:38:44.031000")
    );
  add "ofday_small_diff"
    (fun () ->
      let same x y =
        match x, y with
        | Some x, Some y -> abs_float (x -. y) < sqrt epsilon_float
        | None, None -> true
        | _ -> false
      in
      let check (s1,s2,d) =
        let t1 = Time.Ofday.of_string s1 in
        let t2 = Time.Ofday.of_string s2 in
        same (Option.map ~f:Span.to_sec (Time.Ofday.small_diff t1 t2)) d &&
          same (Option.map ~f:Span.to_sec (Time.Ofday.small_diff t2 t1))
          (Option.map ~f:(~-.) d)
      in
      "foo" @? List.for_all ~f:check
        ["10:00:01.298", "14:59:55.000", Some 6.298;
         "08:59:54.000", "10:00:01.555", Some (-7.555);
         "12:48:55.787", "17:48:55.000", Some 0.787;
        ]);
  add "ofday_occurrence_right_side"
    (fun () ->
      let times = [
        "00:00:00";
        "00:00:01";
        "09:00:00";
        "11:59:59";
        "12:00:00";
        "12:00:01";
        "18:30:30";
        "23:59:59";
      ] in
      let now = Time.now () in
      let now_f = Time.to_float now in
      let utimes = Time.to_ofday now :: List.map times ~f:(Time.Ofday.of_string) in
      let after_times = List.map utimes
        ~f:(fun ut -> Time.ofday_occurrence ut `right_after now) in
      let before_times = List.map utimes
        ~f:(fun ut -> Time.ofday_occurrence ut `right_before now) in
      "right-side-after" @? List.for_all after_times
        ~f:(fun t -> Time.to_float t > now_f);
      "right-side-before" @? List.for_all before_times
        ~f:(fun t -> Time.to_float t < now_f);
    );
  add "ofday_occurrence_distance"
    (fun () ->
      let now = Time.of_string "2007-05-04 13:00:00.000" in
      let after_times = [
        ("13:00:00.000", "2007-05-05 13:00:00.000");
        ("13:00:00.001", "2007-05-04 13:00:00.001");
        ("11:59:59.999", "2007-05-05 11:59:59.999");
        ("00:00:00.000", "2007-05-05 00:00:00.000");
        ("12:59:59.000", "2007-05-05 12:59:59.000");
      ] in
      let before_times = [
        ("13:00:00.000", "2007-05-03 13:00:00.000");
        ("13:00:00.001", "2007-05-03 13:00:00.001");
        ("11:59:59.999", "2007-05-04 11:59:59.999");
        ("00:00:00.000", "2007-05-04 00:00:00.000");
        ("12:59:59.000", "2007-05-04 12:59:59.000");
      ] in
      List.iter after_times ~f:(fun (od_s,prediction_s) ->
        let od = Time.Ofday.of_string od_s in
        let prediction = Time.of_string prediction_s in
        let real = Time.ofday_occurrence od `right_after now in
        ("right-distance - " ^ od_s ^ "," ^ prediction_s) @?
          if Span.to_ms (Time.diff prediction real) = 0. then true
          else false
      );
      List.iter before_times ~f:(fun (od_s,prediction_s) ->
        let od = Time.Ofday.of_string od_s in
        let prediction = Time.of_string prediction_s in
        let real = Time.ofday_occurrence od `right_before now in
        ("right-distance - " ^ od_s ^ "," ^ prediction_s) @?
          if Span.to_ms (Time.diff prediction real) = 0. then true
          else false
      )
    );
  add "diff"
    (fun () ->
      let d1 = Date.create ~y:2000 ~m:Month.jan ~d:1 in
      let d2 = Date.create ~y:2000 ~m:Month.jan ~d:2 in
      let d3 = Date.create ~y:2000 ~m:Month.feb ~d:28 in
      let d4 = Date.create ~y:2000 ~m:Month.mar ~d:1 in
      "normal-diff" @? (Date.diff d2 d1 = 1);
      "leap-diff" @? (Date.diff d4 d3 = 2)
      )
;;


let roundtrip s =
  let t = Span.of_string s in
  ("string roundtrip " ^ s) @?
    Span.(=) t (Span.of_string (Span.to_string t));
  ("span roundtrip " ^ s) @?
    String.(=) s (Span.to_string (Span.of_string s))
;;


let () =
  let extensions = ["ms";"s";"m";"h"] in
  add "roundtrip span<->string" (fun () ->
    List.iter extensions ~f:(fun ext ->
      let t x = roundtrip (x ^ ext) in
      t "1";
      t "5";
      t "1.34";
    );
    let t x = roundtrip (x ^ "s") in
    t "59.9999";
    t "59";
  );
  add "Span.of_string" (fun () ->
    let test string secs =
      ("sec " ^ string) @? (Span.to_sec (Span.of_string string) = secs)
    in
    test "1ms" 0.001;
    test "95ms" 0.095;
    test "1222ms" 1.222;
    test "1.222s" 1.222;
    test "0.5m" 30.;
    test "1m" 60.;
    test "1h" (60. *. 60.);
  );
  add "Time.of_string_fix_proto" (fun () ->
    let test s t =
      ("fix proto time " ^ s) @? (Time.of_string_fix_proto `Utc s = t)
    in
    test
      "20080603-13:55:35.577"
      (Time.of_float (Int64.float_of_bits 4742872407195577745L)))
;;



let test = "time" >::: !test_list
