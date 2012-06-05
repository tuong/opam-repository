(** Experimental base module, which could be a replacement of Pbase. 
    
    Here, the base monad is not using the result monad, but errors are reported by exception.
    So far, there is no big difference of performance between Pbase and Pbaseexn.
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

    type 'a t = Str.t -> 'a * Str.t
  
    let return v = fun st -> (v, st)

    let bind t f = fun st ->
      Profile.incr ();
      let (r, st') = t st in
      f r st'
  end)

  open Open

  let error s : 'a t = fun st -> raise (Exn (Str.position st, s))
  let throw err = raise (Exn err)

  external ok : ('a * Str.t) -> ('a * Str.t) = "%identity"

  let take : Str.elem t = fun s -> match Str.peek s with
    | None -> throw (Str.position s, "unexpected end of stream")
    | Some (elem, _pos, s') -> ok (elem, s')

  let take_ : unit t = fun s -> match Str.peek s with
    | None -> throw (Str.position s, "unexpected end of stream")
    | Some (_, _, s') -> ok ((),s')

  let position : Str.Pos.t t = fun st -> ok (Str.position st, st)

  let critical_error pos s = raise (Critical_error (pos, s))

  let stream : Str.t t = fun st -> ok (st, st)

  let eos : unit t = fun s -> match Str.peek s with
    | Some _ -> throw (Str.position s, "end of stream expected")
    | None -> ok ((), s)

  let (<?>) : 'a t -> string -> 'a t = fun c mes st ->
    try c st with
    | Exn (pos, _) -> throw (pos, "expected " ^ mes)

  let (<?!>) : 'a t -> string -> 'a t = fun c mes st ->
    try c st with
    | Exn (pos, _) -> throw (pos, mes)

  let (<?@>) : 'a t -> Str.Pos.t -> 'a t = fun c pos st ->
    try c st with
    | Exn (_, mes) -> throw (pos, mes)

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

  let tokens : Str.elem list -> unit t = fun elems str ->
    let rec aux str = function
      | [] -> return () str
      | x::xs -> 
          match Str.peek str with
          | Some (c,_,str') when c = x -> aux str' xs
          | _ -> error (Printf.sprintf "expected %s" (Str.show_elem x)) str
    in
    aux str elems
  
  let option : 'a t -> 'a option t = fun com s ->
    try
      let (v, s') = com s in ok (Some v, s')
    with
    | Exn _ -> ok (None, s)

  let option_ : 'a t -> unit t = fun com s ->
    try
      let (_v, s') = com s in ok ((), s')
    with
    | Exn _ -> ok ((), s)

  let result : 'a t -> ('a, error) Result.t t = fun c st ->
    try 
      let res, st = c st in
      ok (Ok res, st)
    with
    | Exn e -> ok (Error e, st)

  let try_finally : 'a t -> (('a, error) Result.t -> unit) -> 'a t = fun c f -> 
    result c >>= fun res ->
    f res;
    match res with
    | Ok v -> return v
    | Error err -> fun _st -> throw err

  let ( ?** ) : 'a t -> 'a list t = fun com ->
    let rec aux st = fun s ->
      try 
        let (v, s') = com s in
        aux (v :: st) s'
      with
      | Exn _ -> (List.rev st, s)
    in
    aux []
  
  let ( ?* ) : 'a t -> unit t = fun com ->
    let rec aux = fun s ->
      try 
        let _v, s' = com s  in
        aux s'
      with
      | Exn _ -> (), s
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
  
  let critical : 'a t -> 'a t = fun t st ->
    try t st with Exn err -> raise (Critical_error err)

  let (<|>) : 'a t -> 'a t -> 'a t = fun c1 c2 st -> 
    let pos0 = Str.position st in
    try c1 st with Exn (pos, _) when pos = pos0 -> c2 st

  let try_ : 'a t -> 'a t = fun c st ->
    let pos0 = Str.position st in
    try c st with
    | Exn (_, err) -> throw (pos0, err)

  let (<!>) : 'a t -> 'a t -> 'a t = fun c1 c2 st -> 
    Profile.start ();
    let pos = Str.position st in
    try 
      let res = c1 st in
      Profile.add (Profile.stop ());
      res
    with Exn (pos', _) ->
      if pos = pos' then Profile.add (Profile.stop ())
      else begin
        Profile.wasted := !Profile.wasted + Profile.stop ();
      end;
      c2 st

  let (<<>) : 'a t -> 'a t -> 'a t = fun c1 c2 st ->
    let pos = Str.position st in
    try 
      let res1 = c1 st in
      Profile.add (Profile.stop ());
      res1
    with
    | (Exn (pos', _) as err1) ->
        let binds_in_c1 = Profile.stop () in
        Profile.start (); 
        try
          let res2 = c2 st in
          if pos = pos' then Profile.add binds_in_c1
          else Profile.wasted := !Profile.wasted + binds_in_c1;
          res2
        with Exn (pos'', _) ->
          if pos' = pos'' then Profile.add (Profile.stop ())
          else Profile.wasted := !Profile.wasted + Profile.stop ();
          raise err1

  let eos_as_none : 'a t -> 'a option t = fun t ->
    (eos >>= fun _ -> return None)
    <|> (t >>= fun x -> return (Some x))

  (* Used for push back *)          
  let (<&>) : 'a t -> ('a -> 'b t) -> 'b t = fun c1 c2 -> 
    fun st ->
      let v, _ = c1 st in
      c2 v st


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

  let set_stream new_st _st = ok ((), new_st)

  let run t st = try Ok (t st) with Exn err -> Error err
end
