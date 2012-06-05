
#require "archimedes";;

let p = A.init [];;
A.Axes.box p;;
A.fx p (fun x -> x *. x) (-1.) 1.;;

A.close p;;
