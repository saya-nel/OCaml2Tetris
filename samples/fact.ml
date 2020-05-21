external print_int : int -> unit = "caml_print_int" ;;

let rec fact = function 0 -> 1 | n -> n * fact (n - 1) in print_int (fact 6)