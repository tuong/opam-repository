(* A string value to print *)
let s = "String value";;

(* A floating point value to print *)
let f = 3.1415;;

(* "interpolate" directive *)
print_endline "Testing interpolate directive";;
print_endline interpolate "String:       $s";;
print_endline interpolate "String again: $(s)";;
print_endline interpolate "Float:        ${f, % 6.2f}";;
print_endline interpolate "Expression with printf: ${1 + 2 + 3, %d}";;

(* Quotations *)
print_endline "\nTesting quotations";;
print_endline <:here<String:           $s $s>>;;
print_endline <:here<String again:     $(s) $(s)>>;;
print_endline <:here<Simple function:  ${f, string_of_float} ${f, string_of_float}>>;;
print_endline <:here<printf directive: ${f, % 6.2f} ${f, % 6.2f}>>;;
print_endline <:here<Ignore the given value: ${f, fun _ -> "anything"} ${f, fun _ -> "anything"}>>;;
let desc = "Value from expression";;
print_endline <:here<$(desc): ${4 + 5 + 6 + 7, fun x -> string_of_int x} ${4 + 5 + 6 + 7, fun x -> string_of_int x}>>;;

(* Other checks and tests *)
print_endline <:here<A bare dollar sign: $>>;;
print_endline interpolate "A bare dollar sign: $500.00";;
print_endline interpolate "A new line \
with no newline";;
print_endline interpolate "An escaped dollar sign: $$";;
print_endline interpolate "An escaped dollar sign with a value: $$s";;
print_endline interpolate "An escaped dollar sign with a value: $$(s)";;
print_endline interpolate "An escaped dollar sign with a value: $${s, you_should_see_me}";;
print_endline <:here<String literal in a printing function: ${s, fun s -> s ^ "!"}>>;;
print_endline <:here<Interpolation in a quotation : ${s, fun s -> String.copy interpolate "$s"}>>;;

(* Batteries *)

#use "topfind";;
#require "batteries";;
open Batteries_uni;;

print_endline "\nTesting Batteries";;
let desc = "Integer list";;
let l = [1;2;3];;
print_endline <:batt<$(desc): ${l, List.print Int.print} ${l, List.print Int.print}>>;;
