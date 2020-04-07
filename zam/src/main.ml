
(* interpreter le programme d'entrée *)

(* let _ = Interp.interp Input.code *)


(* interpreter le programme de test numéro i *)
let test i =
  print_string "test "; 
  print_int i; 
  print_newline ();
  Interp.interp Check.code.(i) ;;

let _ = test 62


