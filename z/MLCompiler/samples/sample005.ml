(* devrait afficher 42 *)

let test x = 
	print_int (if let x = let y = 5 in y + 2 in x = 7
	           then 42 else 0);
	print_newline ()
