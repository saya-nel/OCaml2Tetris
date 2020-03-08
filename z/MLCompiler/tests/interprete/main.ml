type instr = 
 | PUSH
 | POP
 | ACC
 | PUSHACC
 | ASSIGN

(* let debug pc =
	begin
	  print_string "pc  ";
      print_int pc;
	  print_newline () 
    end *)

let main () = 
	let pc = ref 0 in
	let sp = ref 0 in
	let accu = ref 0 in
	let env = ref 0 in
	let extraargs = ref 0 in
	let trasp = ref 0 in
	let globaldata = ref 0 in
	let bytecode = [|PUSH;POP;ACC|] in
	while !pc < Array.length bytecode do
			pc := !pc + 1;
			print_int (Array.length bytecode); 
            print_newline ();
			print_int !pc; 
            print_newline ();
			()
    done