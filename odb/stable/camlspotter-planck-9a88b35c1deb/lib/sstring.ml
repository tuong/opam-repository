open Spotlib.Spot
open Stream_intf

include Stream.Make(struct
  type elem = string
  let show_elem = Printf.sprintf "%S"
  let equal_elem (x : string) y = x = y
  type attr = unit
  let default_attr = ()
  module Pos = Position.None
  let position_of_attr () = ()
end)

let from_string str = Lazy.lazy_from_val (cons_desc str () (null ()))

let from_chan ic =
  let len = 1024 in
  let rec read () = lazy begin
    let buf = String.create len in
    let read_bytes = input ic buf 0 len in
    if read_bytes = 0 then null_desc ()
    else
      let str = String.sub buf 0 read_bytes in
      cons_desc str () (read ())
  end
  in
  read ()
