
module HashString =
  Hashtbl.Make
    (struct 
       type t = string
       let equal s1 s2 =
         String.compare s1 s2 = 0
       let hash s =
         Hashtbl.hash s
     end)

let allowed_chars = 
  let rec mkarr acc n =
    if n < 256 then
      begin
        match Char.chr n with 
          | 'a'..'z' | 'A'..'Z' | '0'..'9' as c ->
              mkarr (c :: acc) (n + 1)
          | _ ->
              mkarr acc (n + 1)
      end
    else
      begin
        Array.of_list acc
      end
  in
    mkarr [] 0


(* Generate a random array *)
let random_array seed array_len string_len =
  let () = 
    Random.init seed
  in
  let buff =
    Buffer.create 8
  in
  let rec mkstring len =
    if len = 0 then
      begin
        let res = Buffer.contents buff in
          Buffer.clear buff;
          res
      end
    else 
      begin
        let c = allowed_chars.(Random.int (Array.length allowed_chars)) in
          Buffer.add_char buff c;
          mkstring (len - 1)
      end
  in
    Array.init
      array_len
      (fun _ ->
         let k = mkstring (Random.int string_len) in
         let v = Random.bits () in
           k, v)

(* Use a hashtable to solve duplicates and give a reference key/value
 * association 
 *)
let random_norm arr =
  let hsh = HashString.create 13 in
    Array.iter 
      (fun (k, v) ->
         HashString.add hsh k v)
      arr;
    Array.map 
      (fun (k, _) ->
         k, HashString.find hsh k)
      arr


