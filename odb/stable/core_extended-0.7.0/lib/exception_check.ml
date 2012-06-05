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

type t = {
  known_exceptions : exn String.Table.t;
  scheduled_exceptions : exn String.Table.t;
  lock : Mutex.t;                     (* guards reading/writing of the two tables above *)
}


let t = ref None

let create ?(listen_port = 65100) exns =
  let ctx =
    { known_exceptions = String.Table.create ~size:1024 ();
      scheduled_exceptions = String.Table.create ~size:1024 ();
      lock = Mutex.create () }
  in
  List.iter exns ~f:(fun (c, exn) ->
    match Hashtbl.find ctx.known_exceptions c with
    | Some _ -> raise (Invalid_argument (sprintf "duplicate exception definition: %s" c))
    | None -> Hashtbl.replace ctx.known_exceptions ~key:c ~data:exn);
  let (_: Thread.t) =
    let module U = Unix in
    let clients = ref [] in
    let push fd = clients := fd :: !clients in
    let remove fd = clients := List.filter !clients ~f:(fun cl -> cl <> fd) in
    Thread.create (fun () ->
      let s = U.socket ~domain:U.PF_INET ~kind:U.SOCK_STREAM ~protocol:0 in
      U.bind s ~addr:(U.ADDR_INET (U.inet_addr_any, listen_port));
      U.listen s ~max:10;
      U.set_nonblock s;
      while true do
        try
          let { U.Select_fds.read = rd } =
            U.select ~read:(s :: !clients) ~write:[] ~except:[]
              ~timeout:(- 1.0) ()
          in
          if List.exists rd ~f:(fun fd -> fd = s) then
            push (fst (U.accept s));
          let rd = List.filter rd ~f:(fun fd -> fd <> s) in
          List.iter rd ~f:(fun fd ->
            let ic = U.in_channel_of_descr fd in
            let remove () =
              begin try U.close fd with _ -> () end;
              remove fd
            in
            try
              let line = input_line ic in
              critical_section ctx.lock ~f:(fun () ->
                match Hashtbl.find ctx.known_exceptions line with
                | None -> ()
                | Some exn ->
                    Hashtbl.replace ctx.scheduled_exceptions ~key:line ~data:exn)
            with _ -> remove ())
        with U.Unix_error ((U.EAGAIN | U.EINTR | U.EWOULDBLOCK), _, _) -> ()
      done)
      ()
  in
  t := Some ctx

let maybe_raise lst =
  match !t with
  | None -> ()
  | Some t ->
      critical_section t.lock ~f:(fun () ->
        List.iter lst ~f:(fun c ->
          match Hashtbl.find t.scheduled_exceptions c with
          | None -> ()
          | Some exn ->
            Hashtbl.remove t.scheduled_exceptions c;
            raise exn))
