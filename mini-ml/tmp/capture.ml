let f x y = print_int x; print_int y

let _ =
  let x = 5 in 
  f x (let x = 6 in x)