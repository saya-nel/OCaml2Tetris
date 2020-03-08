let main x = 
	let i = ref 5 in
	while !i < 10 do 
	  (print_int !i) ;
	  (print_newline ());
	  (i := !i + 1)
    done