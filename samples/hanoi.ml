external print_int : int -> unit = "caml_print_int" ;;
external print_int : int -> unit = "caml_print_newline" ;;


let rec hanoi n i j =
 if n > 0 then begin
  hanoi (n - 1) i (6 - (i + j));
  print_int i;
  print_newline ();
  print_int j;
  print_newline ();
  print_newline ();
  hanoi (n - 1) (6 - (i + j)) j;
 end in
   (hanoi 6 10) 

(* ~> ne fonctionne pas. à revoir. *)