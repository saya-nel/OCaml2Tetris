type foo = (int -> int -> int * int)

type t = A | B | C | D

let check_references () = 
  assert ((match C with A -> 1 | B -> 2 | C -> 42 | D -> 3) = 42);
  assert ((match 5 with 5 -> 42) = 42);
  assert ((match 5 with _ -> 42) = 42);
  assert ((match 5 with 5 -> 42 | _ -> 17) = 42);
  assert ((match 5 with 2 -> 18 | 5 -> 42 | _ -> 17) = 42);
  assert ((match 5 with 2 -> 18 | 5 -> 42 | 1 -> 62 | _ -> 17) = 42);
  match 3 with
  | 2 -> print_string "2"
  | 1 -> print_string "1"
  | 5 -> print_string "1"
  | 3 -> print_string "ok"
  | 6 -> print_string "2"
  | 8 -> print_string "2"
  | _ -> print_string "ko"
  (* assert ((Array.make 4 17).(1) = 17); 
  assert (let x = ref 42 in !x = 42);
  assert (let x = ref 42 in x := 17; !x = 17);
  assert (let x = ref 42 in let y = x in y := 17; !x = 17);
  assert (let x = ref 42 in let y = x in x := 17; !y = 17);
  assert (let x = ref 42 in incr x; !x = 43);
  assert (let x = ref 42 in decr x; !x = 41);
  assert (let x = ref 42 in let y = x in incr x; x = y && !x = !y);
  
  print_string "ok" ;
  print_char 'A'
*)
let () = 
  check_references ()