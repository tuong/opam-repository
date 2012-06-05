open Spotlib.Spot
open Planck_intf

module Extend(Sbuffer : sig
  include Stream_intf.S
  with type elem = char
  and  type Pos.t = Position.File.t
  val substr : t -> int -> int -> string * t
  val takeWhile : (char -> bool) -> t -> string * t
  val bytes : t -> int
end)(Base : Planck_intf.S
     with type Str.elem = Sbuffer.elem
     and  type Str.attr = Sbuffer.attr
     and  type Str.Pos.t = Sbuffer.Pos.t
    ) = struct

  open Base

  (* No reason not having Pchar extensions *)

  include Pchar.Extend(Sbuffer)(Base)

  let bytes = stream >>= fun stream -> return (Sbuffer.bytes stream)

  let prefix n = 
    stream >>= fun str ->
    let pos = Sbuffer.bytes str in
    try
      let s, str' = Sbuffer.substr str pos n in
      set_stream str' >>= fun () -> 
      return s
    with
    | _ -> error "unexpected end of stream"

  let takeWhile p =
    stream >>= fun str ->
    let s, str' = Sbuffer.takeWhile p str in
    set_stream str' >>= fun () ->
    return s

  let ( ??** ) = takeWhile
  let ( ??* ) p = ignore (takeWhile p)

  let string s =
    position >>= fun pos0 ->
    result (prefix (String.length s)) >>= function
      | Result.Ok s' when s = s' -> return ()
      | _ -> throw (pos0, Printf.sprintf "expected %S" s)

  let matched : unit t -> string t = fun t ->
    stream >>= fun stream ->
    bytes >>= fun pos_start ->
    t >>= fun () -> 
    bytes >>= fun pos_end ->
    return (fst (Sbuffer.substr stream pos_start (pos_end - pos_start)))

  let with_matched : 'a t -> ('a * string) t = fun t ->
    stream >>= fun stream ->
    bytes >>= fun pos_start ->
    t >>= fun res -> 
    bytes >>= fun pos_end ->
    return (res, fst (Sbuffer.substr stream pos_start (pos_end - pos_start)))

  (** longest match *)
  let (</>) : 'a t -> 'a t -> 'a t = fun t1 t2 ->
    position >>= fun pos0 ->
    (result (t1 >>= fun res -> stream >>= fun str -> return (res, str)))
      <&> (fun res1 ->
        result (t2 >>= fun res -> stream >>= fun str -> return (res, str)) >>= fun res2 ->
        match res1, res2 with
        | Result.Ok (res, str), Result.Error _ 
        | Result.Error _, Result.Ok (res, str) -> set_stream str >>= fun () -> return res
        | Result.Ok (res1, str1), Result.Ok (res2, str2) -> 
            if Sbuffer.bytes str1 >= Sbuffer.bytes str2 then set_stream str1 >>= fun () -> return res1
            else  set_stream str2 >>= fun () -> return res2
        | Result.Error (_,err1), Result.Error (_,err2) ->
            error (err1 ^ " or " ^ err2) <?@> pos0)

end
