
let rec fact acc = function 
| 0 -> acc
| n -> fact (n * acc) (n - 1) 
 
 in N2t.print_int (fact 1 6)