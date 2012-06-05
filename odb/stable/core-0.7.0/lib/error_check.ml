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

open Sexplib.Conv

module Toggle = struct
  type ('param, 'alert) t =
    {
      assertion: ('param -> bool);
      fail_alert: ('param -> 'alert);
      success_alert: ('param -> 'alert);
      mutable success_prior: bool;
    }
  let create ~assertion ~fail_alert ~success_alert =
    {
      fail_alert    = fail_alert;
      success_alert = success_alert;
      assertion     = assertion;
      success_prior  = true;
    }

  let check ec param =
    if ec.assertion param then
      (* assertion succeeded *)
      if ec.success_prior then
        (* and it succeeded last time too -- no change *)
        None
      else
        (* it did not succeed last time -- change to success *)
        begin
          ec.success_prior <- true;
          Some (ec.success_alert param)
        end
    else
      (* assertion failed *)
      if ec.success_prior then
        (* but succeeded last time -- change to failure *)
        begin
          ec.success_prior <- false;
          Some (ec.fail_alert param)
        end
      else
        (* but failed last time -- no change *)
        None

  let state ec = ec.success_prior
  let sexp_of_t _ _ ec = sexp_of_string (if state ec then "GOOD" else "BAD")
end

module ToggleN = struct
  type ('param, 'alert) t =
    {
      assertion: ('param -> bool);
      fail_alert: ('param -> 'alert);
      final_fail_alert: ('param -> 'alert);
      success_alert: ('param -> 'alert);
      max_consecutive_fail_alerts: int;
      mutable consecutive_fail_alert_count: int;
    }
  let create ~assertion ~fail_alert ~final_fail_alert ~success_alert
    ~max_consecutive_fail_alerts =
    {
      assertion        = assertion;
      fail_alert       = fail_alert;
      final_fail_alert = final_fail_alert;
      success_alert    = success_alert;
      max_consecutive_fail_alerts  = max_consecutive_fail_alerts;
      consecutive_fail_alert_count  = 0;
    }

  let check ec param =
    if ec.assertion param then
      (* assertion succeeded *)
      if ec.consecutive_fail_alert_count = 0 then
        (* and it succeeded last time too -- no change *)
        None
      else
        (* it did not succeed last time -- change to success *)
        begin
          ec.consecutive_fail_alert_count <- 0;
          Some (ec.success_alert param)
        end
    else
      (* assertion failed *)
      if ec.consecutive_fail_alert_count = ec.max_consecutive_fail_alerts then
        (* we're at max_consecutive_fail_alerts now *)
        begin
          ec.consecutive_fail_alert_count <- ec.consecutive_fail_alert_count + 1;
          Some (ec.final_fail_alert param)
        end
      else
        if ec.consecutive_fail_alert_count < ec.max_consecutive_fail_alerts then
        (* haven't reached max_consecutive_fail_alerts yet *)
        begin
          ec.consecutive_fail_alert_count <- ec.consecutive_fail_alert_count + 1;
          Some (ec.fail_alert param)
        end
      else
        (* we're beyond max_consecutive_fail_alerts *)
        None

  let state ec = ec.consecutive_fail_alert_count = 0
  let sexp_of_t _ _ ec = sexp_of_string (if state ec then "GOOD" else "BAD")
end

module Timer = struct
  type ('param, 'alert) t =
    {
      assertion: ('param -> bool);
      fail_alert: ('param -> 'alert);
      success_alert: ('param -> 'alert);

      min_alert_interval: Span.t;
      mutable prior_fail_alert_time : Time.t option;
    }
  let create ~assertion ~fail_alert ~success_alert ~min_alert_interval =
    {
      fail_alert = fail_alert;
      success_alert = success_alert;
      assertion = assertion;
      min_alert_interval = min_alert_interval;
      prior_fail_alert_time = None;
    }
  let check ec param now =
    if ec.assertion param then
      (* assertion succeeded *)
      match ec.prior_fail_alert_time with
      | None ->
          (* and it succeeded prior time too -- no change *)
          None
      | Some _ ->
          (* it did not succeed last time -- change to success *)
          begin
            ec.prior_fail_alert_time <- None;
            Some (ec.success_alert param)
          end
    else
      (* assertion failed *)
      let fail_alert () =
        ec.prior_fail_alert_time <- Some now;
        Some (ec.fail_alert param)
      in
      match ec.prior_fail_alert_time with
      | None -> fail_alert ()
      | Some t ->
          if Span.(>) (Time.abs_diff now t) ec.min_alert_interval then
            (* enough time has passed since last failure, so alert *)
            fail_alert ()
          else
            None
  let state ec = ec.prior_fail_alert_time = None
  let sexp_of_t _ _ ec = sexp_of_string (if state ec then "GOOD" else "BAD")
end

module Step = struct
  type ('threshold, 'param, 'alert) t =
    {
      initial_threshold: 'threshold;
      mutable threshold: 'threshold;

      mutable in_middle_of_cycle : bool;
      adjust: ('param -> threshold:'threshold -> 'threshold);
      assertion: ('param -> threshold:'threshold -> bool);
      fail_alert: ('param -> 'alert);
      success_alert: ('param -> 'alert);
    }
  let create ~threshold ~adjust ~assertion ~fail_alert ~success_alert =
    {
      initial_threshold = threshold;
      threshold = threshold;
      in_middle_of_cycle = false;
      adjust = adjust;
      fail_alert = fail_alert;
      success_alert = success_alert;
      assertion = assertion;
    }

  let check ec param =
    if ec.assertion param ~threshold:ec.initial_threshold then (* assertion succeeded *)
      if not ec.in_middle_of_cycle then
        (* and it succeeded before -- do nothing *)
        None
      else
        (* but it did not succeed before *)
        begin
          ec.threshold <- ec.initial_threshold;
          ec.in_middle_of_cycle <- false;
          Some (ec.success_alert param)
        end
    else (* assertion failed *)
      if ec.assertion param ~threshold:ec.threshold then
        (* but it's not enough worse than before *)
        None
      else
        (* and it's worse than before *)
        begin
          let new_threshold = ec.adjust param ~threshold:ec.threshold in
          ec.in_middle_of_cycle <- true;
          ec.threshold <- new_threshold;
          Some (ec.fail_alert param)
        end
  let state ec = not ec.in_middle_of_cycle
  let sexp_of_t _ _ _ ec = sexp_of_string (if state ec then "GOOD" else "BAD")
end



module ReportAllFails = struct
  type ('param, 'alert) t =
    {
      assertion: ('param -> bool);
      fail_alert: ('param -> 'alert);
      success_alert: ('param -> 'alert);
      mutable success_prior: bool;
    }
  let create ~assertion ~fail_alert ~success_alert =
    {
      fail_alert    = fail_alert;
      success_alert = success_alert;
      assertion     = assertion;
      success_prior  = true;
    }

  let check ec param =
    if ec.assertion param then
      (* assertion succeeded *)
      if ec.success_prior then
        (* and it succeeded last time too -- no change *)
        None
      else
        (* it did not succeed last time -- change to success *)
        begin
          ec.success_prior <- true;
          Some (ec.success_alert param)
        end
    else
      (* assertion failed *)
      begin
        if ec.success_prior then
          (* but succeeded last time -- change to failure *)
          ec.success_prior <- false;
        (* Unlike Toggle, report failure regardless of previous state. *)
        Some (ec.fail_alert param)
      end

  let state ec = ec.success_prior
  let sexp_of_t _ _ ec = sexp_of_string (if state ec then "GOOD" else "BAD")
end
