
type t = | A | B | C | D

let main () = 
	begin
		(match C with
	    | A -> print_int(0)
		| C -> print_int(42)
		| _ -> print_string "else");
	    print_newline ()
    end
