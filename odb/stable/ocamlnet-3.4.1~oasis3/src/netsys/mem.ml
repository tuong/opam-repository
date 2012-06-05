(* Testing *)

#use "topfind";;
#require "netsys";;
open Netsys_mem;;
open Printf;;
let m = alloc_memory_pages 1;;
let () = value_area m;;
let ba = Bigarray.Array1.create Bigarray.int Bigarray.c_layout 5;; 

for k = 0 to 4 do
  ba.{ k } <- 4 + k
done;;

let dump (m : Netsys_mem.memory) =
  let n = Bigarray.Array1.dim m in
  let w = n/8 in
  for k = 0 to w-1 do
    let v = ref 0L in
    for j = 7 downto 0 do
      let c = m.{ 8*k+j } in
      v := Int64.logor (Int64.shift_left !v 8) (Int64.of_int (Char.code c))
    done;
    printf "%d: %016Lx\n" k !v
  done
;;
