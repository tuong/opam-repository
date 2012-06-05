(** Experimental base module, which could be a replacement of Pbase. 
    
    Here, the base monad is not using the result monad, but errors are reported by exception.
    Streams are not given by function arguments but from the global reference [global_stream].
    So far, there is no big difference of performance between Pbase and Pbaseref.
*) 

open Spotlib.Spot
open Planck_intf
open Result

module Make(Str : Stream_intf.S) = struct
  module Str = Str

  type error = Str.Pos.t * string
  exception Critical_error of error
  exception Exn of error

  module Profile = struct
    let all_total = ref 0
    let all_wasted = ref 0
    let total = ref 0
    let wasted = ref 0
    let top_ref = ref (ref 0)
    let stack = ref [!top_ref]
    let start () = 
      top_ref := ref 0;
      stack := !top_ref :: !stack
    let stop () = 
      match !stack with
      | r::r'::rs -> 
          top_ref := r';
          stack := (r'::rs); 
          !r
      | _ -> assert false
        
    let incr () =
      incr total;
      incr !top_ref

    let add n =
      !top_ref := !(!top_ref) + n

    let format ppf = 
      Format.fprintf ppf "Bind total %d, wasted %d, ratio %f"
        !total !wasted (float !wasted /. float !total)

    let reset () =
      all_total := !all_total + !total;
      all_wasted := !all_wasted + !wasted;
      top_ref := ref 0;
      total := 0;
      wasted := 0;
      stack := [!top_ref]

    let recover_all () =
      reset ();
      total := !all_total;
      wasted := !all_wasted
  end

  include Monad.Make(struct 

    type 'a t = unit -> 'a
  
    let return v = fun () -> v

    let bind t f = fun () ->
      Profile.incr ();
      let r = t () in
      f r ()
  end)

  open Open

  let global_stream = ref Str.default_null
  let get () = !global_stream
  let set str = global_stream := str

  let error s : 'a t = fun () -> raise (Exn (Str.position !global_stream, s))
  let throw e = raise (Exn e)
  external ok : 'a -> 'a = "%identity"

  let take : Str.elem t =  fun () -> match Str.peek !global_stream with
    | None -> throw (Str.position !global_stream, "unexpected end of stream")
    | Some (elem, _pos, s') -> 
        set s';
        ok elem

  let take_ : unit t = fun () -> match Str.peek !global_stream with
    | None -> throw (Str.position !global_stream, "unexpected end of stream")
    | Some (_, _, s') -> 
        set s';
        ok ()

  let position : Str.Pos.t t = fun () -> ok (Str.position !global_stream)

  let critical_error pos s = raise (Critical_error (pos, s))

  let stream : Str.t t = fun () -> ok (!global_stream)

  let eos : unit t = fun () -> match Str.peek !global_stream with
    | Some _ -> throw (Str.position !global_stream, "end of stream expected")
    | None -> ok ()

  let (<?>) : 'a t -> string -> 'a t = fun c mes () ->
    let str = get () in
    try c () with
    | Exn (pos, _) -> set str; throw (pos, "expected " ^ mes)

  let (<?!>) : 'a t -> string -> 'a t = fun c mes () ->
    let str = get () in
    try c () with
    | Exn (pos, _) -> set str; throw (pos, mes)

  let (<?@>) : 'a t -> Str.Pos.t -> 'a t = fun c pos () ->
    let str = get () in
    try c () with
    | Exn (_, mes) -> set str; throw (pos, mes)

  (* [take] advances the stream. If we use [take] and some predicate to check the result,
     the error position must be fixed as the one at the use of [take]. *)

  let tokenp : (Str.elem -> bool) -> Str.elem t = fun p ->
    position >>= fun pos ->
    take >>= fun elem -> if p elem then return elem else error "tokenp" <?@> pos

  let token_option : (Str.elem -> 'a option) -> 'a t = fun p ->
    position >>= fun pos ->
    take >>= fun elem -> 
    match p elem with
    | Some v -> return v
    | None -> error "tokenop" <?@> pos

  let token_result : (Str.elem -> ('a, string) Result.t) -> 'a t = fun p ->
    position >>= fun pos ->
    take >>= fun elem ->
    match p elem with 
    | Ok v -> return v
    | Error mes -> error mes <?@> pos

  let token : Str.elem -> unit t = fun tkn ->
    ignore (tokenp (fun x -> tkn = x)) <?>  Str.show_elem tkn

  let one_of : Str.elem list -> Str.elem t = fun tkns ->
    tokenp (fun x -> List.mem x tkns)
    <?> Printf.sprintf "expected one of %s"
         (String.concat " " (List.map Str.show_elem tkns))

  let tokens : Str.elem list -> unit t = fun elems ->
    let rec aux = function
      | [] -> return ()
      | x::xs -> 
          match Str.peek !global_stream with
          | Some (c,_,str') when c = x -> 
              set str';
              aux xs
          | _ -> error (Printf.sprintf "expected %s" (Str.show_elem x))
    in
    aux elems
  
  let option : 'a t -> 'a option t = fun com () ->
    let str = get () in
    try
      let v = com () in ok (Some v)
    with
    | Exn _ -> set str; ok (None)

  let option_ : 'a t -> unit t = fun com () ->
    let str = get () in
    try
      let _v = com () in ok ()
    with
    | Exn _ -> set str; ok ()

  let result : 'a t -> ('a, error) Result.t t = fun c () ->
    let str = get () in
    try 
      let res = c () in
      ok (Ok res)
    with
    | Exn e -> set str; ok (Error e)

  let try_finally : 'a t -> (('a, error) Result.t -> unit) -> 'a t = fun c f -> 
    result c >>= fun res ->
    f res;
    match res with
    | Ok v -> return v
    | Error err -> fun _st -> throw err

  let ( ?** ) : 'a t -> 'a list t = fun com () ->
    let rec aux st = 
      let str = get () in
      try 
        let v = com () in
        aux (v :: st)
      with
      | Exn _ -> set str; List.rev st
    in
    aux []
  
  let ( ?* ) : 'a t -> unit t = fun com ->
    let rec aux = fun () ->
      let str = get () in
      try 
        let _v = com () in
        aux ()
      with
      | Exn _ -> set str; ()
    in
    aux
  
  let (?++) : 'a t -> 'a list t = fun com ->
    com >>= fun v -> 
    ?** com >>= fun vs -> 
    return (v :: vs)
  
  let (?+) : 'a t -> unit t = fun com ->
    com >>= fun _v -> 
    ?* com >>= fun () -> 
    return ()

  let list_with_sep ?(optional_head=false) ~sep t = 
    (if optional_head then option_ sep else return ()) >>= fun () ->
    t >>= fun v -> 
    ?** (sep >>= fun () -> t) >>= fun vs ->
    return (v :: vs)

  let surrounded left right content =
    left >>= fun _ ->
      content >>= fun res ->
        right >>= fun _ ->
  	  return res
  
  let critical : 'a t -> 'a t = fun t () ->
    let str = get () in
    try t () with Exn err -> set str; raise (Critical_error err)

  let (<|>) : 'a t -> 'a t -> 'a t = fun c1 c2 () -> 
    let pos0 = Str.position !global_stream in
    let str = get () in
    try c1 () with Exn (pos, _) when pos = pos0 -> set str; c2 ()

  let try_ : 'a t -> 'a t = fun c () ->
    let pos0 = Str.position !global_stream in
    let str = get () in
    try c () with
    | Exn (_, err) -> set str; throw (pos0, err)

  let (<!>) : 'a t -> 'a t -> 'a t = fun c1 c2 () -> 
    let _pos = Str.position !global_stream in
    let str = get () in
    try 
      c1 ()
    with Exn (_pos', _) ->
      set str;
      c2 ()

  let (<<>) : 'a t -> 'a t -> 'a t = fun c1 c2 () ->
    let _pos = Str.position !global_stream in
    let str = get () in
    try 
      c1 ()
    with
    | (Exn (_pos', _) as err1) ->
        set str;
        try
          c2 ()
        with Exn (_pos'', _) ->
          set str;
          err1

  let eos_as_none : 'a t -> 'a option t = fun t ->
    (eos >>= fun _ -> return None)
    <|> (t >>= fun x -> return (Some x))

  (* Used for push back *)          
  let (<&>) : 'a t -> ('a -> 'b t) -> 'b t = fun c1 c2 () -> 
    let v = c1 () in
    c2 v ()


  let rec (/**/) : 'a list t -> 'a list t -> 'a list t = fun c1 c2 ->
    (c1 >>= fun x -> (/**/) c1 c2 >>= fun y -> return (x @ y)) <!> c2
        
  let rec ( /*/ ) : unit t -> unit t -> unit t = fun c1 c2 ->
    (c1 >>= fun () -> (/*/) c1 c2) <!> c2
        
  let rec (/++/) : 'a list t -> 'a list t -> 'a list t = fun c1 c2 ->
    c1 >>= fun x -> (/**/) c1  c2 >>= fun y -> return (x @ y)

  let rec (/+/) : unit t -> unit t -> unit t = fun c1 c2 ->
    c1 >>= fun () -> (/*/) c1  c2 

  let (/?/) : 'a list t -> 'a list t -> 'a list t = fun c1 c2 ->
    (c1 >>= fun x -> c2 >>= fun y -> return (x @ y)) <!> c2

  let begin_with c1 c2 =
    option c1 >>= function
      | None -> return None
      | Some _ -> c2 >>= fun v -> return (Some v)

  let set_stream new_st _st = set new_st; ok ()

  let run t st = 
    set st;
    try let res = t () in Ok (res, get ()) with Exn err -> Error err
end
