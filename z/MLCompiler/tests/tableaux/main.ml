
let main () = 
	let x = [|40;(40+1);42;43|] in
	let y = x in
	begin
	  x.(0) <- 17;
	  print_int (x.(0));
	  print_int (y.(0));
	  print_int (x.(1));
	  print_int (x.(2));
	  print_int (y.(3));
	  print_newline ()
    end
