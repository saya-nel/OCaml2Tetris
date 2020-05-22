external print_int : int -> unit = "caml_print_int" ;;
external print_string : string -> unit = "caml_print_string" ;;
external print_newline : unit -> unit = "caml_print_newline" ;;

let s = String.make 0 '_' ;;
print_string s;;