external print_int : int -> unit = "caml_print_int" ;;

let x = if false then () else print_int (M1.a + 2)