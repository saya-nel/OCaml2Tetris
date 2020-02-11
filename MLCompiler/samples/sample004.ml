(* devrait afficher 42 *)

let test x = 
	print_int (if let x = 5 in x = 5 then 42 else 0);
	print_newline ()
