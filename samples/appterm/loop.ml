
let rec f = function 
| 0 -> 42
| n -> f (n - 1) 
 
 in N2t.print_int (f 6)