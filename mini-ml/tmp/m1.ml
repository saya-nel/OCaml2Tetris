type t = Q | T of int * int * int 

let print x = 
  match x with 
  | Q -> print_int 42
  | T (a,b,c) -> (print_int (a+b+c))

let x = T 10
let y = x 6
let z = y 1

let _ = print z


(* type t = A
       | B of int
       | C of (int * t * int)

let _ = B 4
let c = C 5
*)

(* 
type option = None | Some of int

let print_opt x = 
  match x with 
  | None -> print_string "(none)"
  | Some(n) -> print_int n

let a = (fun x -> Some x)

let _ = print_opt (a 6)

*)



(*
type t = A
       | B of int
       | C of int => t => int
*)
(*
let c = C 5

let _ = print_int (c A 5)

*)
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