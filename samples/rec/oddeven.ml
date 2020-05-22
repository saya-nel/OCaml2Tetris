
let rec even = function 0 -> true | n -> odd (n-1)
and odd = function 0 -> false | n -> even (n-1) in
if even 42 
then N2t.print_int 1 
else N2t.print_int 0