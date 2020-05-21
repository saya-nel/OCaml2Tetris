external print_int : int -> unit = "caml_print_int" ;;

let f m n = 
  if m = 0 then n else
  if n = 0 then 42 else 84
in
  print_int (f 3 4) 

(* ~> 125 *)    (* là, ne marche pas apparament *)