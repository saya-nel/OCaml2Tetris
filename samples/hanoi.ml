let rec hanoi n i j =
 if n > 0 then begin
  hanoi (n - 1) i (6 - (i + j));
  N2t.print_int i;
  N2t.print_newline ();
  N2t.print_int j;
  N2t.print_newline ();
  N2t.print_newline ();
  hanoi (n - 1) (6 - (i + j)) j;
 end in
   (hanoi 6 10) 

(* ~> ne fonctionne pas. à revoir. *)