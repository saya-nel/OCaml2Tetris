(* devrait afficher 42 *)


let id x = x

let test x =
	print_int (if id 5 = 5 then 42 else 0);
	print_newline ()
