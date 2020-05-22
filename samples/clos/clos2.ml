external print_int : int -> unit = "caml_print_int" ;;

let g x = x + 42 in
let f x = g (x + 1) in print_int (f 4)

(* ~> 47 *)