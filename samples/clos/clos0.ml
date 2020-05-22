external print_int : int -> unit = "caml_print_int" ;;

let f x = x + 3 in print_int (f 39)

(* ~> 47 *)