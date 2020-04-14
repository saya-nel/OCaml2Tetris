let stack_size = 1024

let pc = ref 0

let sp = ref 0
let extra_args = ref 0 
let stack = Array.make stack_size (Mlvalues.val_long 0)

let acc = ref (Mlvalues.val_long 0)
let env = ref (Mlvalues.val_long 0)

let global = ref (Mlvalues.make_block 0 20)

let pop_stack () =
  let v = stack.(!sp - 1) in 
  decr sp;
  v

let push_stack v =
  stack.(!sp) <- v; incr sp

let take_argument code =
  incr pc;
  code.(!pc)


let rec debug_print_block block =
  print_string "(block, size : ";
  print_int (Mlvalues.size (Mlvalues.ptr_val block));
  print_string ", tag : ";
  print_int (Mlvalues.tag (Mlvalues.ptr_val block));
  print_string ") ";
  for i = 0 to Mlvalues.size (Mlvalues.ptr_val block) - 1 do
    print_string "<";
    if Mlvalues.is_ptr (Mlvalues.get_field block i) then
      debug_print_block (Mlvalues.get_field block i)
    else
      print_int (Mlvalues.get_field block i);
    print_string ">";
    print_string " | "
  done;
  print_newline ()

let debug_print_state () = 
  print_newline ();
  print_string " pc: "; 
  print_int (!pc);
  print_newline ();
  print_string " acc: "; 
  if Mlvalues.is_ptr (!acc) then 
    debug_print_block (!acc)
  else 
    print_int (Mlvalues.long_val (!acc));
  print_newline ();
  print_string " env: ";
  if Mlvalues.is_ptr (!env) then
    debug_print_block (!env)
  else 
    print_int (Mlvalues.long_val (!env));
  print_newline ();
  print_string " global: ";
  if Mlvalues.is_ptr (!global) then
    debug_print_block (!global);
  print_newline ();
  print_string " sp: "; 
  print_int (!sp);
  print_newline ();
  print_string " extra args: ";
  print_int (!extra_args);
  print_newline ()

let debug_print_stack () =
  print_newline ();
  print_string "stack :";
  print_newline ();
  for i = 0 to !sp - 1 do  
    print_int (Mlvalues.long_val stack.(i));
    print_string " | "
  done;
  print_newline ()



let interp code = 
 let trap_sp = ref 0 in

  (* env := make_empty_env (); *)
  sp := 0;
  print_int (!acc);

  print_int 42;

  while !pc < Array.length code do
    incr pc where () = 
   (* debug_print_state (); *)
    match code.(!pc) with
    | 0 (* ACC0 *) -> acc := stack.((!sp)-1)
    | _ -> print_string "instruction inconnue. exit."; exit 0
  done;

  print_newline ();
  print_string "fin programme :";
  print_newline ();
  debug_print_state ();
  debug_print_stack ();
  print_newline ()


