

let main =
  let u = 6 in
  print_int ((fun x y -> if true then x else y) 42 (fun a -> u))