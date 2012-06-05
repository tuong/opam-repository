open Spotlib.Spot
open Planck_intf
open Result

(* Interface for generic char stream. Not optimized. *)
module Extend(Str : Stream_intf.S with type elem = char) (Base : Planck_intf.S 
      with type Str.elem = char
      and  type Str.attr = Str.attr
      and  type Str.Pos.t = Str.Pos.t)  = struct

  open Base

  let rec string : string -> unit t = fun str -> 
    let len = String.length str in
    position >>= fun pos0 ->
    let rec aux pos = 
      if pos = len then return ()
      else tokenp (fun c -> c = String.unsafe_get str pos) >>= fun _ -> aux (pos+1)
    in
    aux 0 <?@> pos0 <?> Printf.sprintf "%S" str

  let chars_to_string : char list t -> string t = fun chars ->
    chars >>= fun chars ->
      let len = List.length chars in
      let s = String.create len in
      let rec fill pos = function
	| [] -> s
	| x::xs ->
	    String.unsafe_set s pos x;
	    fill (pos+1) xs
      in
      return (fill 0 chars)
end
