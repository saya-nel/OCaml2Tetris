external print_int : int -> unit = "caml_print_int" ;;

exception E

let _ = try raise E with E -> print_int 42

(* problème, le try se compile en un code avec PUSHGETGLOBAL *)