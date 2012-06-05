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

let raises_exception f =
  try f (); false
  with _ -> true

let null_handler _ _ = ()
let exception_handler _ _ = failwith "printing this exception does not indicate a bug"

let test =
  "timer" >:::
    [ "abusive user" >::
        (fun () ->
           "add" @?
             begin
               let timer = Timer.create () in
               raises_exception
                 (fun () -> Timer.add timer null_handler (Time.Span.of_sec (-1.)))
             end;
           "remove twice" @?
             begin
               let timer = Timer.create () in
               let ev = Timer.add timer null_handler (Time.Span.of_sec 0.1) in
               Timer.remove ev;
               Timer.remove ev;
               true
             end;
           "malicious handler" @?
             begin
               let timer = Timer.create () in
               let _ev = Timer.add timer exception_handler (Time.Span.of_sec 0.1) in
               Time.pause (Time.Span.of_sec 0.3);
               true
             end;
        );
      "ordering of events" >::
        (fun () ->
           let timer = Timer.create () in
           let delays = Array.init 100 ~f:(fun i -> (float i)/.100.) in
           Array.permute delays;
           let queue = Squeue.create 100 in
           let add_handler _ time = Squeue.push queue (Time.to_float time) in
           Array.iter delays
             ~f:(fun delay ->
                   ignore (Timer.add timer add_handler (Time.Span.of_sec delay))
                );
           Time.pause (Time.Span.of_sec 1.2);
           let last = ref (-1.) in
           while Squeue.length queue > 0 do
             let next = Squeue.pop queue in
             "in order" @? (next >=. !last);
             last := next
           done
        );
      "deactivation" >::
        (fun () ->
           let timer = Timer.create () in
           "initially activated" @? Timer.is_activated timer;
           let dont_call_handler _ _ = ("handler called stupidly" @? false) in
           ignore (Timer.add timer dont_call_handler (Time.Span.of_sec 0.25));
           Timer.deactivate timer;
           "can be deactivated" @? not (Timer.is_activated timer);
           Time.pause (Time.Span.of_sec 0.5)
        )
    ]
