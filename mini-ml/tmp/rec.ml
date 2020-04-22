(* let _ = 
  let rec sum n = if n < 0 then 0 else n + sum (n - 1) in print_int (sum 10)*)

let rec fact = fun x -> if x = 0 then 1 else x * fact (x - 1) in print_int (fact 6) ;;

(*


type nat = Z | S of (nat)

let _ = 
  let rec add = (fun n -> fun m -> 
    match n with 
    Z -> m 
    | S(k) -> S(add m k)) 
  in let a = S(S(Z)) 
  in let b = S(S(S(Z)))
  in add a b*)