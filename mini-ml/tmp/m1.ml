 let f x =
  let a = 42 in
  let g y = 
    let h u = u + a in h y + 1 in
  g x 

(* let f x =
  let a = 42 in
	let g y = y in
	g x *)

(* let f x =
	let g x = 
      let h i = i - 1 in 
      h i in g x
*)
(* let rec sum0 n =
	if n < 0 then 0 else n + sum0 (n-1)

let rec sum n acc =
	if n < 0 then acc else sum (n-1) (n + acc) 
and g x = 1
let _ = print_int (sum 10 0) 
*)
(* let f x =
	let a = 10 in 
	a + 32

let _ = 
  10 * f 3 *)

(*
let _ = 
  let x = [||] in
  let x = Array.length x in
  let x = Array.length x in
  let x = Array.length x in
  let x = Array.length x in
  x
*)






(*
let g n = 
  let n2 = n in
  let u = 
    let fin = n2 - 1 in
    for i = 0 to fin do
      Pervasives.print_int i 
    done 
  in 
  if u = () then print_int 42 else print_string "bar" *)