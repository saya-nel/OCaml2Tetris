
let l = 1 :: 2 :: 3 :: 4 :: 5 :: []

let rec iter f = function
| [] -> () 
| h::t-> let () = f h in iter f t ;;

iter N2t.print_int l;;

(* plante. problème avec APPTERM ? *)