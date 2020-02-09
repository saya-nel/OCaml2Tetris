let rec sum n = if n < 1 then 0 else n + sum (n - 1) ;; 

let main x = 
	begin
	  print_int (sum 10);
	  print_newline ()
    end