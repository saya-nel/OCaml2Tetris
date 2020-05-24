let rec f91 n =
 if n > 100 then n - 10
 else f91 (f91 (n + 11))
in 
  N2t.print_int (f91 42)