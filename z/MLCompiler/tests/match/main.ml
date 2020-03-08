
let main () = 
	begin
		(match 41 + 1 with
	    | 17 -> print_int (17)
		| 42 -> print_int (42)
		| _ -> print_string "else");
	    print_newline ()
    end
