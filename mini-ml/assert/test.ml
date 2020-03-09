
let check_references () = 
  assert ((Array.make 4 17).(1) = 17); 
  assert (let x = ref 42 in !x = 42);
  assert (let x = ref 42 in x := 17; !x = 17);
  assert (let x = ref 42 in let y = x in y := 17; !x = 17);
  assert (let x = ref 42 in let y = x in x := 17; !y = 17);
  assert (let x = ref 42 in incr x; !x = 43);
  assert (let x = ref 42 in decr x; !x = 41);
  assert (let x = ref 42 in let y = x in incr x; x = y && !x = !y);
  
  print_string "ok" ;
  print_char 'A'

let () = 
  check_references ()