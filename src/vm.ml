(* etat de la vm *)
let code = [|102; 10; 132; 3; 10; 102; 127; 2; 84; 11; 99; 19; 1; 58; 57; 0|]
let pc = ref 0
let acc = ref 0
let sp = ref 0
let stack = ref [||]

(* instructions de la vm *)
type instr = 
  Push | Pop of int | Const of int | Add | Ori | Eq | 
  Acc of int | Assign of int | Branch of int | 
  Branchif of int | Multi | Leq | Minusi 

(* initialise l'état de la vm *)
let init_vm code_array = 
  pc := 0;
  acc := 0;
  sp := 0;
  code := code_array;
  stack := Array.make 1000 0

let exec () = match (!code).(!pc) with
  | Push -> 
      sp := !sp + 1;
      (!stack).(!sp) <- !acc;
      pc := !pc + 1
  | Const n ->
      acc := n;
      pc := !pc + 1
  | Add ->
      acc := (!stack).(!sp) + !acc;
      sp := !sp - 1;
      pc := !pc + 1
  | Minusi ->
      acc := -(!acc);
      pc := !pc + 1
  | Ori ->
      acc := if (!stack).(!sp) = 0 && !acc = 0 then 0 else 1;
      sp := !sp - 1;
      pc := !pc + 1
  | Eq ->
      acc := if (!stack).(!sp) = !acc then 1 else 0;
      sp := !sp - 1;
      pc := !pc + 1
  | Acc n ->
      if n >= Array.length !stack || n < 0 then failwith "stack pleine"
      else acc := (!stack).(n);
      pc := !pc + 1
  | Assign n ->
      if n >= Array.length !stack || n < 0 then failwith "stack pleine"
      else 
        begin
          (!stack).(n) <- !acc;
          acc := 0
        end;
      pc := !pc + 1
  | Pop n -> 
      if n >= Array.length !stack || n < 0 then failwith "Stack pleine "
      else sp := !sp - n;
      pc := !pc + 1
  | Branch n -> 
      if n >= Array.length !code then failwith "Instr array out of bounds"
      else pc := !pc + n
  | Branchif n ->
      if n >= Array.length !code then failwith "Instr array out of bounds"
      else pc := if !acc = 1 then !pc + n else !pc + 1
  | Multi -> 
      acc := (!stack).(!sp) * !acc;
      sp := !sp - 1;
      pc := !pc + 1
  | Leq -> 
      acc := if (!stack).(!sp) <= !acc then 1 else 0;
      sp := !sp - 1;
      pc := !pc + 1

(* Methodes pour debug *)
let string_of_instr i =
  match i with
  | Push -> "Push"
  | Pop n -> "Pop "^(string_of_int n)
  | Const n -> "Const "^(string_of_int n)
  | Add -> "Add"
  | Ori -> "Ori"
  | Eq -> "Eq"
  | Acc n -> "Acc "^(string_of_int n)
  | Branch n -> "Branch "^(string_of_int n)
  | Branchif n -> "Branchif "^(string_of_int n)
  | Assign n ->  "Assign "^(string_of_int n)
  | Multi ->  "Multi"
  | Leq -> "Leq"
  | Minusi -> "Minusi"

  let string_of_array a =
    "[ "^(Array.fold_right (fun n s -> ((string_of_int n)^" "^s)) a "]")
  
  let string_of_state () =
    "PC = " ^(string_of_int !pc) ^
    ", A = " ^(string_of_int !acc) ^
    ", Stack = "^(string_of_array (Array.sub !stack 0 (!sp + 1)))

  (* lance l'execution de la vm *)
  let rec launch_exec () : unit = 
    if !pc < Array.length !code then
      begin
        print_endline ((string_of_int !pc) ^ " " ^ (string_of_int (Array.length !code)));
        print_endline (string_of_state ());
        print_endline (string_of_instr (!code).(!pc));
        exec ();
        launch_exec ()
      end
    else ()
;;

(* main *)
let () = 
  init_vm [|Const 3; Push; Const 2; Add|];
  launch_exec ();
  print_endline (string_of_state ())
;;

