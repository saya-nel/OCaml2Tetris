type exp = Int of (int) | Add of (exp * exp)

let f e = 
  match e with 
  | Int(n) -> ()
  | Add(e1,e2) -> ()

