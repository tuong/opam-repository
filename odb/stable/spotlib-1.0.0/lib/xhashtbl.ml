let replace_list tbl kvs = 
  List.iter (fun (k,v) ->
    Hashtbl.replace tbl k v) kvs

let of_list size kvs =
  let tbl = Hashtbl.create size in
  List.iter (fun (k,v) ->
    Hashtbl.replace tbl k v) kvs;
  tbl
  
