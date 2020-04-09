
let g n = 
  let n2 = n in
  let u = 
    let fin = n2 - 1 in
    for i = 0 to fin do
      Pervasives.print_int i 
    done 
  in 
  if u = () then print_int 42 else print_string "bar"