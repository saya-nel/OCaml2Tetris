
let rec fact = function 
| 0 -> 1
| n -> n * fact (n - 1)
in N2t.print_int (fact 6)