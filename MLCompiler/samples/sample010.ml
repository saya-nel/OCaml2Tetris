(* devrait afficher 42 *)

type t = A | B | C | D | E

let test x =
	let res = match B with
	| A -> 17
	| B -> 42
	| C -> 28 
	| _ -> 0 in
	begin
	  print_int res;
	  print_newline ()
    end
