(* bibliotheque d'exécution pour mini-ml *)
(* module Pervasives                     *)                  
(*                                       *)
(* Loïc Sylvestre                        *)

let exit (n : int) : unit = 
	Internal.exit n

let ref (x : 'a) : 'a ref = 
   Internal.array_make 1 x

let ref_contents (r : 'a ref) : 'a = 
	Internal.array_get r 0

let ref_set_contents (r : 'a ref) (x : 'a) : unit = 
	Internal.array_set r 0 x

let incr r = 
	ref_set_contents r (ref_contents r + 1)

let decr r = 
	ref_set_contents r (ref_contents r + 1)

let fst (p : 'a * 'b) : 'a = 
	Internal.fst p

let snd (p : 'a * 'b) : 'b = 
	Internal.snd p

let print_int (n : int) : unit = 
	Internal.print_int n

let print_char (c : char) : unit = 
	Internal.print_char (c : char)
let print_newline (u : unit) : unit = 
	Internal.print_newline (u : unit)

let print_string (s : string) : unit = 
	Internal.print_char_array (s : string)

let failwith (msg : string) : unit = 
	print_string msg; 
    print_newline (); 
    exit 1

let abs (n : int) : int = if n >= 0 then n else - n
 