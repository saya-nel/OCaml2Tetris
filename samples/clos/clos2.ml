let g x = x + 42 in
let f x = g (x + 1) in N2t.print_int (f 4)

(* ~> 47 *)