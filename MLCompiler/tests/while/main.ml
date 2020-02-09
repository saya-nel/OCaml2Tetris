let main x = 
	let i = ref 0 in
	while (!i) < 10 do 
      begin
	    print_int (!i);
	    print_newline ();
	    i := ((!i) + 1)
      end
    done