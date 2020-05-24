let f x = x :: []

let _ = match f 42 with 
        | [x] -> N2t.print_int x
        | _ -> ()