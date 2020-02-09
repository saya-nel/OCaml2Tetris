(* devrait afficher 42 *)

let test x = 
	begin
	  print_int (if true then 42 else 0);
	  print_newline ()
    end
