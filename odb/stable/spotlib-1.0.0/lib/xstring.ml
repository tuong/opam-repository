let is_prefix ?from:(pos=0) ~prefix:sub str =
  let str_len = String.length str in
  let sub_len = String.length sub in
  if pos + sub_len > str_len then false
  else 
    let rec iter i = 
      if str.[pos + i] <> sub.[i] then false
      else 
        let i' = i + 1 in
        if i' = sub_len then true
        else iter i'
    in
    iter 0

let index_string_from str pos sub =
  let sub_len = String.length sub in
  if sub_len = 0 then pos 
  else 
    let limit = String.length str - sub_len in
    let rec iter i = 
      if i > limit then raise Not_found
      else if is_prefix str ~from:i ~prefix:sub then i
      else iter (i+1)
    in
    iter pos

let is_postfix ~postfix:sub str =
  is_prefix ~from:(String.length str - String.length sub) ~prefix: sub str
