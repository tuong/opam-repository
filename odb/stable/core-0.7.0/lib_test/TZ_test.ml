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



let () = Random.self_init ()


let my_zone_name,my_tz = TZ.machine_zone_exn ()

(* We don't test Feb 29th because generating proper leap year dates is
  trickier.  Also, there are no time zone changes on leap dates. *)
let month_limits = Map.of_alist_exn [
    1, 31;
    2, 28;
    3, 31;
    4, 30;
    5, 31;
    6, 30;
    7, 31;
    8, 31;
    9, 30;
    10, 31;
    11, 30;
    12, 31
  ]

let random_time () =
  let year = 1970 + (Random.int 67) in
  let month = 1 + (Random.int 12) in
  let day = 1 + (Random.int (Map.find_exn month_limits month)) in
  let hour = Random.int 24 in
  let min = Random.int 60 in
  let sec = Random.int 60 in
  let usec = Random.int 1_000_000 in
  (year,month,day,hour,min,sec,usec)
;;

let random_time_str () =
  let year,month,day,hour,min,sec,usec = random_time () in
  sprintf "%d-%0.2d-%0.2d %0.2d:%0.2d:%0.2d.%0.6d" year month day hour min sec usec
;;

let time_reset1 = "2000-01-01 10:00:00.000"
let time_reset2 = "2000-08-01 10:00:00.000"
let last = ref ""
let test =
  "tz" >::: [
      "string conversions" >:: (fun () ->
          "UTC epoch" @? begin
            let timestr = "1970-01-01 00:00:00.000" in
            let time = TZ.time_of_string (TZ.Zone.find_exn "UTC") timestr in
            Time.to_float time = 0.
          end;
          "Random time strings" @? begin
            (* It turns out that under some circumstances (indeterminate times) glibc will
             * give different responses based on the last time you gave it.  This is not
             * true of our TZ library, so we prime glibc with a base time to make its
             * behavior deterministic.  You can test this behavior yourself in a top level
             * like so:
             * # open Core;;
             * # let ts1 = "2020-04-13 09:09:47.985";;
             * val ts1 : string = "2020-04-13 09:09:47.985"
             * # let ts2 = "2000-01-01 10:00:00.000";;
             * val ts2 : string = "2000-01-01 10:00:00.000"
             * # let ts3 = "1974-10-27 02:27:54.970";;
             * val ts3 : string = "1974-10-27 02:27:54.970"
             * # Time.of_string ts1;;
             * - : Core.Time.stringable = 2020-04-13 09:09:47.985
             * # Time.to_float (Time.of_string ts3);;
             * - : float = 152069274.97
             * # Time.of_string ts2;;
             * - : Core.Time.stringable = 2000-01-01 10:00:00.000
             * # Time.to_float (Time.of_string ts3);;
             * - : float = 152072874.97
             * Note that you get two different floats...
            *)
            let rec loop n =
              if n <= 0 then true
              else
                begin
                  let last_time_str = !last in
                  let orig_time_str = random_time_str () in
                  last := orig_time_str;
                  let (_:Time.t)  = Time.of_string time_reset1 in
                  let time_read1  = Time.of_string orig_time_str in
                  let (_:Time.t)  = Time.of_string time_reset2 in
                  let time_read2  = Time.of_string orig_time_str in
                  let time_str1   = Time.to_string time_read1 in
                  let time_str2   = Time.to_string time_read2 in
                  let tz_time_read = TZ.time_of_string my_tz orig_time_str in
                  let tz_time_str = TZ.string_of_time my_tz tz_time_read in
                  if (cmp_float (Time.to_float time_read1) (Time.to_float tz_time_read) ||
                      cmp_float (Time.to_float time_read2) (Time.to_float tz_time_read))
                     && (time_str1 = tz_time_str) && (time_str2 = tz_time_str)
                  then loop (n - 1)
                  else
                    begin
                      let printf = eprintf in
                      printf "\nRandom times test failed\n";
                      printf "\tLast time string: %s\n" last_time_str;
                      printf "\trandom time string: %s\n" orig_time_str;
                      printf "\tTime module converted it to: %f and %f\n"
                        (Time.to_float time_read1) (Time.to_float time_read2);
                      printf "\tTZ module converted it to: %f\n"
                        (Time.to_float tz_time_read);
                      printf "\tTime module converted it back to: %s and %s\n"
                        time_str1 time_str2;
                      printf "\tTZ module converted it back to: %s\n" tz_time_str;
                      false
                    end
                end
            in
            loop 10000
          end;
          (* This is expensive, and has been run over a larger range, so we limit it to
           * 100 intervals here *)
          "Random epoch times" @? begin
            (*let all_zones =
              List.map ["Europe/London"; "America/New_York"; "Asia/Tokyo"]
                ~f:(fun name -> (name, TZ.Zone.find_exn name))
            in*)
            let all_zones = TZ.initialized_zones () in
            let max_ran = Int.of_float (2. ** 30. -. 1.) in
            let rec loop n =
              if n <= 0 then true
              else
                begin
                  let epoch = Float.of_int (Random.int max_ran) in
                  let t = Time.of_float epoch in
                  let results =
                    List.map all_zones ~f:(fun (zone_name, zone) ->
                      Unix.putenv ~key:"TZ" ~data:zone_name;
                      ignore (Unix.localtime 1000.);
                      let (_:Time.t) = Time.of_string time_reset1 in
                      let tc1 = Time.to_string t in
                      let (_:Time.t) = Time.of_string time_reset2 in
                      let tc2 = Time.to_string t in
                      let tzc = TZ.string_of_time zone t in
                      let tc1_round_trip = Time.of_string tc1 in
                      let tzc_round_trip = Time.of_string tzc in
                      let ok =
                        (tc1 = tzc || tc2 = tzc) && tc1_round_trip = tzc_round_trip
                      in
                      (zone_name, epoch, ok))
                  in
                  match List.find results ~f:(fun (_,_,matched) -> matched = false) with
                  | None -> loop (n - 1)
                  | Some (name,epoch,_) ->
                      printf "\nConversion of %f to timezone %s using native Unix and \
                      TZ did not match\n" epoch name;
                      false
                end
            in
            loop 100
          end
        )
    ]
