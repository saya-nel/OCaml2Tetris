let rec ack m n = 
  if m = 0 then n + 1 else
  if n = 0 then ack (m-1) 1 else
  ack (m-1) (ack m (n-1))
in
  N2t.print_int (ack 3 4) 

(* ~> 125 *)    (* là, ne marche pas apparament *)