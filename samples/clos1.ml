external print_int : int -> unit = "caml_print_int" ;;

let a = 42 in
let b = 38 in
let f x = x + a + b in print_int (f 4)


(* ~> 84 *)