(* interpreter le programme d'entrée *)

let print_start () =
  print_string "START.";
  print_newline ()

let print_end () =
  print_newline ();
  print_string "STOP.";
  print_newline ();
  Interp.debug_print_state ();
  (* Interp.debug_print_stack (); *)
  print_newline ()

let _ = 
  print_start ();
  Interp.interp Input.code;
  print_end ()
  


