open Stream_intf

module Make(P : Base) = struct

  open Lazylist
  include P

  type t = (elem, attr) zlist

  let null = null
  let default_null = null P.default_attr
  let null_desc = null_desc 
  let cons_desc = cons_desc

  let desc = desc
  let peek t = 
    let desc = desc t in
    match desc with
    | Null _ -> None
    | Cons (elem, attr, t') -> Some (elem, attr, t')

  let is_null = is_null
  let attr = attr
    
  let to_list = to_list
  let to_list_with_attrs = to_list_with_attrs

  let iter = iter
  let fold_right = fold_right
  let map = map

  let rev_between = rev_between
  let between = between

  let position t = 
    let desc = desc t in
    match desc with
    | Null attr -> P.position_of_attr attr
    | Cons (_elem, attr, _) -> P.position_of_attr attr

end
