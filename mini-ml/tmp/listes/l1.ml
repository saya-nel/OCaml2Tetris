let l = 42 :: []

let l2 = l @ l

let _ = 
match l with 
| [] -> ()
| x::t -> print_int x

let mapcat f l = List.concat (List.map f l)


let _ = mapcat (fun x -> x + 1) ("foo" :: [])