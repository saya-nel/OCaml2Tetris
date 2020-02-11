(* devrait afficher 42 *)

let test x = 
	let a = [|(40+2);(40+3);(40+4);(40+5)|] in
	print_int (if a.(0) = 42 && a.(1) = 43 && a.(2) = 44 && a.(3) = 45 
	           then 42 else 0);
	print_newline ()
