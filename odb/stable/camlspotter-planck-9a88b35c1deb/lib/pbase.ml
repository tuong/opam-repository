open Spotlib.Spot
open Planck_intf
open Result

module Make(Str : Stream_intf.S) = struct
  module Str = Str

  type error = Str.Pos.t * string
  exception Critical_error of error

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

    type 'a t = Str.t -> ('a * Str.t, error) Result.t
  
    let return v = fun st -> Ok (v, st)
  
    let bind t f = fun st ->
      Profile.incr ();
      match t st with
      | Ok (r, st') -> f r st' 
      | Error s -> Error s
  end)

  open Open

  let take : Str.elem t = fun s -> 
    Profile.incr ();
    match Str.peek s with
    | None -> Error (Str.position s, "unexpected end of stream")
    | Some (elem, _pos, s') -> Ok (elem, s')

  let take_ : unit t = fun s -> 
    Profile.incr ();
    match Str.peek s with
    | None -> Error (Str.position s, "unexpected end of stream")
    | Some (_, _, s') -> Ok ((),s')

  let position : Str.Pos.t t = fun st -> Profile.incr (); Ok (Str.position st, st)

  let error s : 'a t = fun st -> Profile.incr (); Error (Str.position st, s)
  let throw err = fun _st -> Profile.incr (); Error err

  let critical_error pos s = raise (Critical_error (pos, s))

  let stream : Str.t t = fun st -> Profile.incr (); Ok (st, st)

  let eos : unit t = fun s -> 
    Profile.incr ();
    match Str.peek s with
    | Some _ -> Error (Str.position s, "end of stream expected")
    | None -> Ok ((), s)

  let (<?>) : 'a t -> string -> 'a t = fun c mes st ->
    Profile.incr ();
    let res = c st in
    match res with
    | Ok _ -> res
    | Error (pos, _) -> Error (pos, "expected " ^ mes)

  let (<?!>) : 'a t -> string -> 'a t = fun c mes st ->
    Profile.incr ();
    let res = c st in
    match res with
    | Ok _ -> res
    | Error (pos, _) -> Error (pos, mes)

  let (<?@>) : 'a t -> Str.Pos.t -> 'a t = fun c pos st ->
    Profile.incr ();
    let res = c st in
    match res with
    | Ok _ -> res
    | Error (_, mes) -> Error (pos, mes)

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
    | Error err -> error err <?@> pos

  let token : Str.elem -> unit t = fun tkn ->
    ignore (tokenp (fun x -> Str.equal_elem tkn x)) <?>  Str.show_elem tkn

  (* CR jfuruss: optimization oppotunity for char *)     
  let one_of : Str.elem list -> Str.elem t = fun tkns ->
    tokenp (fun x -> List.exists (Str.equal_elem x) tkns)
    <?> Printf.sprintf "expected one of %s"
         (String.concat " " (List.map Str.show_elem tkns))

  let tokens : Str.elem list -> unit t = fun elems str ->
    Profile.incr ();
    let rec aux str = function
      | [] -> return () str
      | x::xs -> 
          match Str.peek str with
          | Some (c,_,str') when Str.equal_elem c x -> aux str' xs
          | _ -> error (Printf.sprintf "expected %s" (Str.show_elem x)) str
    in
    aux str elems
  
  let option : 'a t -> 'a option t = fun com s ->
    Profile.incr ();
    match com s with
    | Error _ -> return None s
    | Ok (v, s') -> return (Some v) s'

  let option_ : unit t -> unit t = fun com s ->
    Profile.incr ();
    match com s with
    | Error _ -> return () s
    | Ok ((), s') -> return () s'

  let result : 'a t -> ('a, error) Result.t t = fun c st ->
    Profile.incr ();
    match c st with
    | Ok (res, st) -> Ok (Ok res, st)
    | Error e -> Ok (Error e, st)

  let try_finally : 'a t -> (('a, error) Result.t -> unit) -> 'a t = fun c f -> 
    result c >>= fun res ->
    f res;
    match res with
    | Ok v -> return v
    | Error err -> fun _st -> Error err

  let ( ?** ) : 'a t -> 'a list t = fun com ->
    let rec aux st = fun s ->
      Profile.incr ();
      match com s with
      | Error _ -> return (List.rev st) s
      | Ok (v, s') -> aux (v :: st) s'
    in
    aux []
  
  let ( ?* ) : 'a t -> unit t = fun com ->
    let rec aux = fun s ->
      Profile.incr ();
      match com s with
      | Error _ -> return () s
      | Ok (_v, s') -> aux  s'
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
    Profile.incr ();
    match t st with
    | (Ok _ as res) -> res
    | Error (pos, s) -> raise (Critical_error (pos, s))

  let (<|>) : 'a t -> 'a t -> 'a t = fun c1 c2 st -> 
    Profile.incr ();
    let pos0 = Str.position st in
    let res = c1 st in
    match res with
    | Ok _ -> res
    | Error (pos, _) -> if pos = pos0 then c2 st else res

  (* CR jfuruse: _ is used in a different meaning than option_ *)
  let try_ : 'a t -> 'a t = fun c st ->
    Profile.incr ();
    let pos0 = Str.position st in
    let res = c st in
    match res with
    | Ok _ -> res
    | Error (_, err) -> Error (pos0, err)

  let (<!>) : 'a t -> 'a t -> 'a t = fun c1 c2 st -> 
    Profile.incr ();
    Profile.start ();
    let pos = Str.position st in
    match c1 st with
    | (Ok _ as res) -> 
        Profile.add (Profile.stop ());
        res
    | Error (pos', _) -> 
        if pos = pos' then Profile.add (Profile.stop ())
        else begin
          Profile.wasted := !Profile.wasted + Profile.stop ();
        end;
        c2 st

  let (<<>) : 'a t -> 'a t -> 'a t = fun c1 c2 st ->
    Profile.incr ();
    let pos = Str.position st in
    let res1 = c1 st in
    match res1 with
    | Ok _ -> 
        Profile.add (Profile.stop ());
        res1
    | Error (pos', _) ->
        let binds_in_c1 = Profile.stop () in
        Profile.start (); 
        let res2 = c2 st in
        match res2 with
        | Ok _ -> 
            if pos = pos' then Profile.add binds_in_c1
            else Profile.wasted := !Profile.wasted + binds_in_c1;
            res2
        | Error (pos'', _) -> 
            if pos' = pos'' then Profile.add (Profile.stop ())
            else Profile.wasted := !Profile.wasted + Profile.stop ();
            res1

  let eos_as_none : 'a t -> 'a option t = fun t ->
    (eos >>= fun _ -> return None)
    <|> (t >>= fun x -> return (Some x))

  (* Used for push back *)          
  let (<&>) : 'a t -> ('a -> 'b t) -> 'b t = fun c1 c2 -> 
    fun st ->
      Profile.incr ();
      match c1 st with
      | Ok (v, _) -> c2 v st
      | (Error _ as res) -> res


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

  let set_stream new_st _st = Profile.incr (); Ok ((), new_st)

  let run : 'a t -> Str.t -> ('a * Str.t, error) Result.t = fun t st ->
    t st
end
