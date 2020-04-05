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

let f x = 
	let g = (fun x -> print_int x) in
	g x

let _ = f 17