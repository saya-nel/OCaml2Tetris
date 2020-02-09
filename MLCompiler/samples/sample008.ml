(* devrait afficher 42 *)

let test x = 
	let a = [|1;2;3;4|] in
	begin
	  print_int (if Array.length a = 4 then 42 else 0);
	  print_newline ()
    end
