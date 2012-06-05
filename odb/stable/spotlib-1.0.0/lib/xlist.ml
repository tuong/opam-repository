open List

let iteri f l =
  let rec iter n = function
    | [] -> ()
    | x::xs -> f n x; iter (n+1) xs
  in
  iter 0 l

let mapi f l = 
  let rec map n = function
    | [] -> []
    | x::xs -> f n x :: map (n+1) xs
  in
  map 0 l

let from_to f t =
  let rec from_to st f t =
    if f > t then rev st
    else from_to (f::st) (f+1) t
  in
  from_to [] f t
