external print_int : int -> unit = "caml_print_int" ;;

let rec fib n =
 if n <= 1 then 1
 else fib (n - 1) + fib (n - 2) 
in 
 print_int (fib 10)

 (* ~> 89 *)