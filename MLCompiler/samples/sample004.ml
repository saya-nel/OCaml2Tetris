(* devrait afficher 42 *)

let test x = 
	begin
	  print_int (if let x = 5 in x = 5 then 42 else 0);
	  print_newline ()
    end
