(* devrait afficher 42 *)

let test x = 
	begin
	  print_int (if 3 + 2 = 5 then 42 else 0);
	  print_newline ()
    end
