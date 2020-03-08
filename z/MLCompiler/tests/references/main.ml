
let main () = 
	let x = ref 42 in
	let y = x in
	begin
	  y := 17;
	  print_int (!x);
	  print_newline ()
    end
