
open RadixTree 
open Common

module MapString = Map.Make(String)

let () = 
  let loop_hashstring t arr =
    for i = 0 to (Array.length arr) - 1 do 
      let k, v =
        Array.unsafe_get arr i 
      in
        HashString.add t k v
    done
  in
  let test_one arr =
    Benchmark.tabulate
      (Benchmark.latencyN
         5L
         [
           "IString",
           (fun arr ->
              let t = IString.create () in
                for i = 0 to (Array.length arr) - 1 do 
                  let k, v =
                    Array.unsafe_get arr i 
                  in
                    IString.add t k v
                done),
           arr;

           "MapString",
           (fun arr ->
              let rec add t pos =
                if pos < Array.length arr then
                  begin
                    let k, v =
                      Array.unsafe_get arr pos
                    in
                      add
                        (MapString.add k v t)
                        (pos + 1)
                  end
                else
                  t
              in
              let _t : int MapString.t =
                add MapString.empty 0
              in
                ()),
           arr;

           "PString",
           (fun arr ->
              let rec add t pos =
                if pos < Array.length arr then
                  begin
                    let k, v =
                      Array.unsafe_get arr pos
                    in
                      add
                        (PString.add k v t)
                        (pos + 1)
                  end
                else
                  t
              in
              let _t : int PString.t = 
                add PString.empty 0
              in
                ()),
           arr;

           "Hashtbl",
           (fun arr ->
              let t = Hashtbl.create 13 in 
                for i = 0 to (Array.length arr) - 1 do 
                  let k, v =
                    Array.unsafe_get arr i 
                  in
                    Hashtbl.add t k v
                done),
           arr;

           "HashString", 
           (fun arr ->
              let t = HashString.create 13 in
                loop_hashstring t arr),
           arr;

           "HashStringNoResize",
           (fun arr ->
              let t = HashString.create (Array.length arr) in
                loop_hashstring t arr),
           arr;
         ])
  in

  let arr32 =
    random_array 124567 1_000_000 32 
  in
  let arr8 = 
    random_array 12345 1_000_000 8
  in
    Printf.printf "\n\n32 chars: \n%!";
    test_one arr32;
    Printf.printf "\n\n8 chars: \n%!";
    test_one arr8
