let y = (fun y -> y)

let x = 
  let a = 42 in
  let b = 10 in
  print_int ((fun x -> (fun y -> b + (fun z -> a + x + y + z) 10) 1) 17)
  (* print_int (((fun x -> (fun y -> x + a)) 17) 2) *)

(*let print_array = (fun b ->
  print_string "[";
  for i = 0 to Array.length b - 1 do
    print_int b.(0);
    print_string ","
  done;
  print_string "]") ;;



let _ = 
  let a = 42 in
  let b = 24 in
  let c = 18 in
   print_int ((fun x -> let h = a in let w = 5 in let f = (fun y -> w) in print_int f; x) 4)
 
*)
 (* ((((fun x -> print_int x;
             fun y -> print_int a)) 99) 22) *)
 (* print_int (# ((((fun x -> (fun y -> (fun z -> x))) 4) 2) 1)) *)
  (* print_int ((fun x -> (fun y -> let z = b in x + a)) 17 2) *)

(* let _ = 
  let a = 42 in
  ((fun x -> fun y -> x + y + a) 42 1) *)
(*
let g = (fun x -> print_int x)
let h = (fun x -> print_int (x*2))

let f x =
  (if x < 0 then g else h) 42

let _ = f (17) 
*)
(*
 let _ =
   let a = 32 in
   (fun x -> print_int (a+x)) 10
*)


(*
let _ = 
  ((fun x -> fun y -> print_int (x + y)) 42) 1

*)
(*
let _ = 
  ((fun x -> fun y -> print_int (x + y)) 42) 1
*)
(*
let un f = f 1

let _ = un (fun x -> print_int (x + 41)) 
*)
(*
let _ = un (fun x -> print_int (x + 41)) 
*)
(*

let rec aux acc n =
  if n = 0 then acc else aux (n * acc) (n - 1)

let fact n =
  aux 1 n 

let _ = fact 6
*)
(* let compose = (fun f -> (fun g -> f (g x)))

 let f x = 5

 let g x = 17 + x
*)


 (* let f a = Array.iter (fun x -> Pervasives.print_int x) a

 let _ = f [|1;2;3;4|]*)

 (*
 let f x =
  let a = 42 in
  let g y = 
    let h u = u + a in h y + 1 in
  g x 
*)
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