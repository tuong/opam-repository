(* 
   ocamlc -annot -I +camlp4 dynlink.cma camlp4lib.cma -pp camlp4of.opt pa_hash.ml  
   ocaml dynlink.cma camlp4o.cma pa_hash.cmo
*)
module Id = struct
  let name = "pa_inline"
  let version = "1.0"
end

open Camlp4
open PreCast

(* See tutorial at 
   http://ambassadortothecomputers.blogspot.com/2010/03/reading-camlp4-part-5-filters.html 
   http://brion.inria.fr/gallium/index.php/Camlp4MapGenerator
*)

module Make (AstFilters : Sig.AstFilters) = struct
  open AstFilters

  module Normal = struct
    let bind _loc t f =
      match f with
      | <:expr< fun $lid:x$ $lid:st$ -> Profile.incr (); $e$ >> ->
          (* bind t (fun x -> (fun st' -> Profile.incr (); e[x,st'])) ===
               fun st ->
                 match t st  with
                 | Ok (x, st') -> e[x,st']
                 | Error s -> Error s
          *)
          <:expr< fun st__ -> 
            (* Profile.incr (); *)
            match $t$ st__ with 
            | Result.Ok ($lid:x$, $lid:st$) -> $e$
            | (Result.Error _ as res) -> Obj.magic res >>
      | <:expr< fun $lid:x$ $lid:st$ -> $e$ >> ->
          (* bind t (fun x -> (fun st' -> e[x,st'])) ===
               fun st ->
                 match t st  with
                 | Ok (x, st') -> e[x,st']
                 | Error s -> Error s
          *)
          <:expr< fun st__ -> 
            (* Profile.incr (); *)
            match $t$ st__ with 
            | Result.Ok ($lid:x$, $lid:st$) -> $e$
            | (Result.Error _ as res) -> Obj.magic res >>
      | <:expr< fun $lid:x$ -> $e$ >> ->
          (* bind t (fun x -> e[x]) ===
               fun st ->
                 match t st with
                 | Ok (x, st') -> e[x] st'
                 | Error s -> Error s
          *)
          <:expr< fun st__ -> 
            (* Profile.incr (); *)
            match $t$ st__ with 
            | Result.Ok ($lid:x$, st__') -> $e$ st__'
            | (Result.Error _ as res) -> Obj.magic res >>
      | _ ->
          (* bind t f ===
               fun st ->
                 match t st with
                 | Ok (r, st') -> f r st'
                 | Error s -> Error s         <--- Use Obj.magic to avoid creating a new block
          *)
          <:expr< fun st__ -> 
            (* Profile.incr (); *)
            match $t$ st__ with 
            | Result.Ok (r__, st__') -> $f$ r__ st__'
            | (Result.Error _ as res) -> Obj.magic res >>
  
    (* return v st = Ok (v, st) *)
    let return _loc t = <:expr< fun st__ -> (* Profile.incr (); *) Result.Ok ($t$, st__) >>
    let return_st _loc t st = <:expr< Result.Ok ($t$, $st$) >>

(*
  let (<|>) : 'a t -> 'a t -> 'a t = fun c1 c2 st -> 
    let pos0 = Str.position st in
    let res = c1 st in
    match res with
    | Ok _ -> res
    | Error (pos, _) -> if pos = pos0 then c2 st else res
*)

(* (fun stx -> e1) <|> (fun sty -> e2) ===

      fun (stx as sty) -> 
        let pos0 = Str.position stx in
        match e1 with
        | Ok _ -> res
        | Error (pos, _) -> if pos = pos0 then e2 else res
*)

  let (<|>) _loc t1 t2 =
    match t1, t2 with
    | <:expr< fun st__ -> $e1$ >>, <:expr< fun st__ -> $e2$ >> ->
        let shorten e = match e with
            | <:expr< let pos__ = Str.position st__ in $e$ >> -> e
            | _ -> e
        in
        let e1 = shorten e1 in
        let e2 = shorten e2 in
        <:expr< fun st__ ->
          let pos__ = Str.position st__ in
          let res__ = $e1$ in
          match res__ with
          | Result.Ok _ -> res__
          | Result.Error (pos__', _) when pos__ <> pos__' -> res__
          | _ -> $e2$ >>
    | _ ->  <:expr< (<|>) $t1$ $t2$ >>
(* It's too huge
        <:expr< 
          fun st__ ->
            let pos__ = Str.position st__ in
            let res__ = $t1$ st__ in
            match res__ with
            | Ok _ -> res__
            | Error (pos__', _) when pos__ <> pos__' -> res__
            | _ -> $t2$ st__
        >>
*)
  end

  module Exn = struct
    let bind _loc t f =
      match f with
      | <:expr< fun $lid:x$ $lid:y$ -> $e$ >> ->
          (* bind t (fun x -> (fun y -> e[x,y])) ===
               fun st -> let (x, y) = t st in e[x,y]
          *)
          <:expr< fun st__ -> 
            let ($lid:x$, $lid:y$) = $t$ st__ in $e$ >>
      | <:expr< fun $lid:x$ -> $e$ >> ->
          (* bind t (fun x -> e[x]) ===
               fun st ->
                 let (x, st') = t st in e[x] st'
          *)
          <:expr< fun st__ -> 
            let ($lid:x$, st__') = $t$ st in $e$ st__' >>
      | _ ->
          (* bind t f ===
               fun st -> let (r, st') = t st in f r st'
          *)
          <:expr< fun st__ -> 
            let (r__, st__') = $t$ st__ in $f$ r__ st__' >>

    let return _loc t = <:expr< fun st__ -> ($t$, st__) >>
    let return_st _loc t st = <:expr< ($t$, $st$) >>

  (*
    let (<|>) : 'a t -> 'a t -> 'a t = fun c1 c2 st -> 
      let pos0 = Str.position st in
      let res = c1 st in
      match res with
      | Ok _ -> res
      | Error (pos, _) -> if pos = pos0 then c2 st else res
  *)
  
  (* (fun st -> e1[st]) <|> (fun st -> e2[st]) ===
  
        fun st -> 
          let pos0 = Str.position st in
          try e1[st] with Exn (pos, _) when if pos = pos0 -> e2[st]
  *)
  
    let (<|>) _loc t1 t2 =
      match t1, t2 with
      | <:expr< fun st__ -> $e1$ >>, <:expr< fun st__ -> $e2$ >> ->
          let shorten e = match e with
              | <:expr< let pos__ = Str.position st__ in $e$ >> -> e
              | _ -> e
          in
          let e1 = shorten e1 in
          let e2 = shorten e2 in
          <:expr< fun st__ ->
            let pos__ = Str.position st__ in
            try $e1$ with Exn (pos, _) when pos = pos0 -> $e2$ >>

      | _ ->  <:expr< (<|>) $t1$ $t2$ >>
  (* It's too huge
          <:expr< 
            fun st__ ->
              let pos__ = Str.position st__ in
              let res__ = $t1$ st__ in
              match res__ with
              | Ok _ -> res__
              | Error (pos__', _) when pos__ <> pos__' -> res__
              | _ -> $t2$ st__
          >>
  *)
  end

  open Normal

  let _ = 
    let simplify = object
      inherit Ast.map as super
      method! expr e =
        match super#expr e with
(* No inlining. If you want to try it, uncomment the following
        | <:expr@_loc< $lid:x$ $t$ $f$ >> when x = ">>=" || x = "bind" -> bind _loc t f
        | <:expr@_loc< return $t$ $st$>> -> return_st _loc t st
        | <:expr@_loc< return $t$>> -> return _loc t
        | <:expr@_loc< (<|>) dummy $t$ >>-> t
        | <:expr@_loc< $lid:x$ $t$ $f$ >> when x = "<|>" -> (<|>) _loc t f
        | <:expr@_loc< __must_be_unit; $t$ >>-> t
*)
(* If you do not want to count closure creations
        | <:expr@_loc< Profile.incr (); $e$ >>-> <:expr< (); $e$ >>
*)
        | x -> x
    end
    in AstFilters.register_str_item_filter simplify#str_item

end

let module M = Register.AstFilter(Id)(Make) in ()
