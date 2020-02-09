(* devrait afficher 42 *)

let rec sum n = if n < 1 then 0 else n + sum (n - 1) ;; 

let test x = 
	begin
	  print_int (if (sum 10) = 55 then 42 else 0);
	  print_newline ()
    end