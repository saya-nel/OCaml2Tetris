
(* les expressions *)

let assert_expr () = 
	print_string "primitives ";
	assert (true);
	assert (not false);
	assert (1 = 1);
	assert (1 = 1);
	assert (() = ());
	assert (true = true);
	assert (false = false);
	(* assert ([||] = [||]);  pas d'égalité sur les blocs *)
	assert (1 < 2);
	assert (1 <= 2);
	assert (2 <= 2);
	assert (2 >= 1);
	assert (2 >= 2);
	assert (1 + 2 = 3);
	assert (2 - 1 = 1);
	assert (true && true);
	assert (true || true);
	assert (true || false);
	assert (false || true);
	print_string "alternative ";
    assert (if true then true else false);
    assert (if false then false else true);
	print_string "references ";
	assert (let x = ref 1 in !x = 1);
	assert (let x = ref 1 in x := 2; !x = 2);
	assert (let x = ref 1 in let y = x in x := 2; !y = 2);
	print_string "tableaux ";
	assert (Array.length [|17;18;19|] = 3);
	assert ([|17;18;19|].(0) = 17);
	assert ([|42;43;44|].(1) = 43);
	assert ([|7;6;5|].(2) = 5);
	print_string "boucles ";
	assert (let i = ref 0 in while !i < 10 do i := !i + 1 done; !i = 10)

(* les declarations *)

type t = A | B | C ;; 
let f x y = x;;
let rec sum n = if n < 1 then 0 else n + sum (n - 1) ;; 

let assert_decl () = 
	assert (A = A);
	assert (B = B);
	assert (C = C);
	assert (f 1 2 = 1); 
    assert (0 + 1 = f 1 2); 
	assert (sum 10 = 55);
	assert (match B with
	        | A -> false
	        | B -> true
	        | _ -> true) ;;

let main () = 
	assert_expr ();
	assert_decl ()