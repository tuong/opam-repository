open Monad_intf

module Make(M:S) : T with type 'a t = 'a M.t = struct
  include M

  let map ~f t = bind t (fun x -> return (f x))

  module Open_ = struct
    let bind = M.bind
    let (>>=) = M.bind
    let (>>|) t f = map ~f t
    let return = return

    (* Applicative style *)
    let ( ^<$> ) f t = map ~f t 
    let ( /<*> ) : ('a -> 'b) t -> 'a t -> 'b t = fun f a ->
      f >>= fun f -> 
      a >>= fun a ->
      return (f a)
  end
  include Open_
  module Open = struct
    type 'a t = 'a M.t
    include Open_
  end

  let ignore a = a >>= fun _ -> return ()
  let void = ignore

  let rec seq = function
    | [] -> return []
    | x::xs -> 
        x >>= fun x -> 
        seq xs >>= fun xs ->
        return (x::xs)

  let rec seq_ = function
    | [] -> return ()
    | x::xs -> x >>= fun () -> seq_ xs

  let mapM f ls = seq (List.map f ls)

  let rec for_ i to_ f =
    if i > to_ then return ()
    else f i >>= fun () -> for_ (i+1) to_ f
    
  let iteri f ls = seq_ (Xlist.mapi f ls)

end

module Make2(M:S2) : T2 with type ('a, 'z) t = ('a, 'z) M.t = struct
  include M

  let map ~f t = bind t (fun x -> return (f x))

  module Open_ = struct
    let bind = M.bind
    let (>>=) = M.bind
    let (>>|) t f = map ~f t
    let return = return

    (* Applicative style *)
    let ( ^<$> ) f t = map ~f t 
    let ( /<*> ) = fun f a ->
      f >>= fun f -> 
      a >>= fun a ->
      return (f a)
  end
  include Open_
  module Open = struct
    type ('a, 'z) t = ('a, 'z) M.t
    include Open_
  end

  let ignore a = a >>= fun _ -> return ()
  let void = ignore

  let rec seq = function
    | [] -> return []
    | x::xs -> 
        x >>= fun x -> 
        seq xs >>= fun xs ->
        return (x::xs)

  let rec seq_unit = function
    | [] -> return ()
    | x::xs -> x >>= fun () -> seq_unit xs

  let mapM f ls = seq (List.map f ls)

  let rec for_ i to_ f =
    if i > to_ then return ()
    else f i >>= fun () -> for_ (i+1) to_ f
    
  let iteri f ls = seq_unit (Xlist.mapi f ls)

end

