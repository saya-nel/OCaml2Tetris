(* let f x = snd (x,'c')

let _ = print_char @@ f 42

let g = exit 1 *)

(*
let id x = (M.x : t)

let a = 4 + (3 : int) *)
(*
let _ : int = 5 + y where y = 6

let x = let ((y : int)) = 2 in y = 1
*)
(*
let f = (fun x -> (x + 2))

let g = (fun x -> (x * 6))

let compose f1 f2 x = f1 (f2 x)

let _ = print_int (compose f g 17) *)
(*
let f x = 
	let g = (fun x -> print_int x) in
	g x

let _ = f 17*)

(*
let _ = 
	let a = [|1;2;3;4;5|] in
	let a2 = Array.map (fun x -> x + 10) a in
	Array.iter (fun x -> print_int x) a2*)

let f = (fun x -> fun y -> x + y)

let g = f 42
let _ = print_int (g 3)