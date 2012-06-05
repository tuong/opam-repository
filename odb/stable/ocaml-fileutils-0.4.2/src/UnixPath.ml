
open FilePath_type;;

include CommonPath;;

let rec dir_writer lst = 
  match lst with 
      Root s :: tl ->
        "/"^(dir_writer tl)
    | [ CurrentDir Short ] ->
        ""
    | lst ->
        let rec dir_writer_aux cmp =
          match cmp with
              Root _ -> ""
            | ParentDir -> ".."
            | CurrentDir _ -> "."
            | Component s -> s
        in
          String.concat "/" ( List.map dir_writer_aux lst )
;;

let dir_reader fn =
  let sep =
    '/'
  in

  let fn_part_of_string =
    function
      | "." -> CurrentDir Long
      | ".." -> ParentDir
      | str -> Component str
  in

    if (String.length fn) > 0 then
      (
        if fn.[0] = sep then
          StringExt.split
            ~start_acc:[Root ""] 
            ~start_pos:1
            ~map:fn_part_of_string
            sep 
            fn
        else
          StringExt.split 
            ~map:fn_part_of_string
            sep 
            fn
      )
    else
      (
        [CurrentDir Short]
      )
;;

let path_writer lst = 
  String.concat ":" lst
;;

let path_reader str = 
  StringExt.split ~map:(fun s -> s) ':' str
;;

let fast_concat fn1 fn2 =
  let fn1_len =
    String.length fn1
  in
    if fn1_len = 0 || fn1.[fn1_len - 1] = '/' then
      fn1 ^ fn2
    else
      fn1 ^ "/" ^ fn2
;;

let fast_basename fn =
  try
    let start_pos = 
      (String.rindex fn '/') + 1
    in
    let fn_len =
      String.length fn
    in
      if start_pos = fn_len then
        ""
      else
        String.sub fn start_pos (fn_len - start_pos) 
  with Not_found ->
    fn
;;

let fast_dirname fn =
  try
    let last_pos = 
      String.rindex fn '/'
    in
      if last_pos = 0 then
        "/"
      else
        String.sub fn 0 last_pos
  with Not_found ->
    ""
;;

let fast_is_relative fn =
  if String.length fn = 0 || fn.[0] <> '/' then
    true
  else 
    false
;;

let fast_is_current fn =
  if String.length fn = 0 || fn = "." then
    true
  else if fn.[0] <> '.' then
    false
  else
    raise CannotHandleFast
;;

let fast_is_parent fn =
  if fn = ".." then
    true
  else if String.length fn < 2 || fn.[0] <> '.' || fn.[1] <> '.' then
    false
  else
    raise CannotHandleFast
;;
