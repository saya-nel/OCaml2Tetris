let code = [|103;12345;143|] (* [|102; 10; 132; 3; 10; 102; 127; 2; 84; 11; 99; 19; 1; 58; 57; 0|] *)

let stack_size = 1024

let stack = Array.make stack_size 0
let pc = ref 0
let acc = ref 0
let sp = ref 0

let debug_print_state () = 
  print_string " pc: "; 
  print_int (!pc);
  print_string " acc: "; 
  print_int (!acc);
  print_string " sp: "; 
  print_int (!sp);
  print_string "\n"

let interp () =
  while !pc >= 0 do
    debug_print_state ();
    match code.(!pc) with
    | 0 (* ACC0 *) -> acc := stack.(0); incr pc
    | 1 (* ACC1 *) -> acc := stack.(1); incr pc
    | 2 (* ACC2 *) -> acc := stack.(2); incr pc
    | 3 (* ACC3 *) -> acc := stack.(3); incr pc    
    | 4 (* ACC4 *) -> acc := stack.(4); incr pc
    | 5 (* ACC5 *) -> acc := stack.(5); incr pc
    | 6 (* ACC6 *) -> acc := stack.(6); incr pc
    | 7 (* ACC7 *) -> acc := stack.(7); incr pc   
    | 8 (* ACC *) -> 
       incr pc;
       let n = code.(!pc) in
       if n >= Array.length stack || n < 0 then failwith "stack pleine"
       else acc := stack.(n);
       incr pc
    | 9 (* Push *) -> 
      incr sp; stack.(!sp) <- !acc; incr pc
    | 10 (* PUSHACC0 (~ PUSH) *) -> 
      incr sp; stack.(!sp) <- !acc; incr pc
    | 11 (* PUSHACC1 *) -> 
      incr sp; stack.(!sp) <- !acc; acc := stack.(1); incr pc
    | 12 (* PUSHACC2 *) -> 
      incr sp; stack.(!sp) <- !acc; acc := stack.(2); incr pc
    | 13 (* PUSHACC3 *) -> 
      incr sp; stack.(!sp) <- !acc; acc := stack.(3); incr pc
    | 14 (* PUSHACC4 *) -> 
      incr sp; stack.(!sp) <- !acc; acc := stack.(4); incr pc
    | 15 (* PUSHACC5 *) -> 
      incr sp; stack.(!sp) <- !acc; acc := stack.(5); incr pc
    | 16 (* PUSHACC6 *) -> 
      incr sp; stack.(!sp) <- !acc; acc := stack.(6); incr pc
    | 17 (* PUSHACC7 *) -> 
      incr sp; stack.(!sp) <- !acc; acc := stack.(7); incr pc
    | 18 (* PUSHACC *) -> 
      incr pc; 
      let n = code.(!pc) in
      incr sp; stack.(!sp) <- !acc; acc := stack.(n); incr pc
    | 103 (* ConstInt *) -> 
       incr pc;
       let n = code.(!pc) in
       acc := n;
       incr pc
    | 110 (* ADDINT *) ->  
       acc := stack.(!sp) + !acc;
       decr sp;
       incr pc
    | 109 (*NEGINT*) -> 
       acc := -(!acc);
       incr pc
    | 116 (* Ori *) -> 
       acc := if stack.(!sp) = 0 && !acc = 0 then 0 else 1;
       decr sp;
       incr pc
    | 121 (* Eq *) ->
       acc := if (stack).(!sp) = !acc then 1 else 0;
       decr sp;
       incr pc
    | 20 (* Assign *) -> 
       incr pc;
       let n = code.(!pc) in 
       if n >= Array.length stack || n < 0 then failwith "stack pleine"
       else begin stack.(n) <- !acc;
                  acc := 0
            end;
       incr pc
    | 19 (* Pop *) -> 
       incr pc;
       let n = code.(!pc) in 
       if n >= Array.length stack || n < 0 then failwith "Stack pleine "
       else sp := !sp - n;
       incr pc
    | 84 (* Branch *) -> 
       incr pc;
       let n = code.(!pc) in 
       if n >= Array.length code then failwith "Instr array out of bounds"
       else pc := !pc + n
    | 85 (* Branchif *) -> 
       incr pc;
       let n = code.(!pc) in 
       if n >= Array.length code then failwith "Instr array out of bounds"
       else pc := if !acc = 1 then !pc + n else !pc + 1
    | 112 (* MULINT *) -> 
       acc := stack.(!sp) * !acc;
       decr sp;
       incr pc
    | 124 (* LEINT *) -> 
       acc := if stack.(!sp) <= !acc then 1 else 0;
       decr sp;
       incr pc
    | 143 (* STOP *) -> pc := -1
  done

let () =
  interp ()
