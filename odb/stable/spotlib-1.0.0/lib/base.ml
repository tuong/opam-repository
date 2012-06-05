let failwithf fmt = Printf.kprintf failwith fmt

let memoize f =
  let cache = Hashtbl.create 101 in
  fun v -> try Hashtbl.find cache v with Not_found ->
    let r = f v in
    Hashtbl.replace cache v r;
    r

let (^.) f g = fun x -> f (g x)
let (^$) f x = f x

let protect f a ~finally =
  match try `Ok (f a) with e -> `Exn e with
  | `Ok v -> finally (); v
  | `Exn e -> finally (); raise e

let with_time f v =
  let start = Unix.gettimeofday () in
  let res = f v in
  let end_ = Unix.gettimeofday () in
  res, end_ -. start

