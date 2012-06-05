
open OUnit
open RadixTree

let assert_find find str e =
  assert_equal 
    ~msg:(Printf.sprintf "lookup %S" str)
    ~printer:string_of_int
    (find str)
    e

let test_random_pstring seed arr_len str_len = 
  let arr = Common.random_array seed arr_len str_len in
  let arr_exp = Common.random_norm arr in
  let t = 
    Array.fold_left
      (fun t (k, v) ->
         PString.add k v t)
      PString.empty 
      arr
  in
    Array.iter 
      (fun (k, v) -> 
         assert_find 
           (fun k -> PString.find k t)
           k v)
      arr_exp

let test_random_istring seed arr_len str_len =
  let arr = Common.random_array seed arr_len str_len in
  let arr_exp = Common.random_norm arr in
  let t = IString.create () in
    Array.iter 
      (fun (k, v) ->
         IString.add t k v)
      arr;
    Array.iter 
      (fun (k, v) -> 
         assert_find 
           (fun k -> IString.find t k)
           k v)
      arr_exp

let _lst : test_result list = 
  run_test_tt_main 
    ("RadixTree" >:::
     [
       "PString" >:::
       [
         "Simple" >::
         (fun () ->
            let t = 
              List.fold_left
                (fun t (k, v) ->
                   PString.add k v t)
                PString.empty
                ["abcde", 1; 
                 "defgh", 2;
                 "", 3]
            in
              assert_find (fun k -> PString.find k t) "abcde" 1;
              assert_find (fun k -> PString.find k t) "defgh" 2;
              assert_find (fun k -> PString.find k t) "" 3;
              assert_raises Not_found (fun () -> PString.find "zzz" t));

         "20" >::
         (fun () ->
            test_random_pstring 123456 20 32);

         "1000" >::
         (fun () -> 
            test_random_pstring 123456 1000 32);
       ];

       "IString" >::
       (fun () ->
          test_random_istring 123456 1000 32);
     ])

