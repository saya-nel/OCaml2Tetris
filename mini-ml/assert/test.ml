
let () = 
  assert (let x = ref 42 in !x = 42);
  assert (let x = ref 42 in x := 17; !x = 17);
  assert (let x = ref 42 in let y = x in y := 17; !x = 17);
  assert (let x = ref 42 in let y = x in x := 17; !y = 17);
  assert (let x = ref 42 in incr x; !x = 43);
  assert (let x = ref 42 in decr x; !x = 41);
  Output.printString "good"


(* les expressions *)
(*
let f n v = 
	let a = Array.create_uninitialized n in
	for i = 0 to n do Array.set a i v done;
	a

let assert_expr () = 
	let v = [|42;43;44;45|] in
    let a = Array.make 1000 17 in
     Array.set a 2 (v.(2)); 
     Array.get a 2;
     let x = ref 2 in
     Pervasives.incr x;
     Output.printInt(!x)
  
let () = 
  assert_expr () *)