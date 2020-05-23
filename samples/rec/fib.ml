
let rec fib n =
 if n <= 1 then 1
 else fib (n - 1) + fib (n - 2) 
in 
 N2t.print_int (fib 10)

 (* ~> 89 *)