open Lazy

type ('a, 'attr) zlist = ('a, 'attr) desc lazy_t

and ('a, 'attr) desc =
  | Cons of 'a * 'attr * ('a, 'attr) zlist
  | Null of 'attr

let null attr = lazy_from_val (Null attr)
let null_desc attr = Null attr
let cons_desc v attr t = Cons (v, attr, t)

let desc = force
let peek t = 
  match desc t with
  | Null _ -> None
  | Cons (v, attr, t') -> Some (v, attr, t')

let is_null t = match desc t with
  | Null _ -> true
  | _ -> false

let attr t = match desc t with
  | Null a -> a
  | Cons (_, a, _) -> a

let to_list t = 
  let rec to_list st t = match desc t with
    | Null _ -> List.rev st
    | Cons (elem, _, t) -> to_list (elem :: st) t
  in
  to_list [] t
  
let to_list_with_attrs t = 
  let rec to_list_with_attrs st t = 
    let desc = desc t in
    match desc with
    | Null _ -> List.rev st
    | Cons (elem, attr, t) -> 
        to_list_with_attrs ((elem, attr) :: st) t
  in
  to_list_with_attrs [] t
  
let rec iter f t = 
  let desc = desc t in
  match desc with
  | Null attr -> f None attr
  | Cons (elem, attr, t) -> f (Some elem) attr; iter f t

let rec fold_right f lst st =
  match desc lst with
  | Null attr -> f None attr st
  | Cons (v, attr, lst') -> fold_right f lst' (f (Some v) attr st)

let rec map fcons fnull lst =
  lazy (match desc lst with
  | Null attr -> Null (fnull attr)
  | Cons (v, attr, lst') ->
      let v, attr = fcons v attr in
      Cons (v, attr, map fcons fnull lst'))

(* [t2] must be a postfix of [t1] otherwise, it loops forever *)
let rev_between t1 t2 =
  let rec loop st t =
    if t == t2 then st (* CR jfuruse: we cannot always use pointer eq *)
    else 
      let desc = desc t in
      match desc with
      | Cons (elem, attr, t') -> loop ((elem, attr)::st) t'
      | Null _ -> st
  in
  loop [] t1

let between t1 t2 = List.rev (rev_between t1 t2)

