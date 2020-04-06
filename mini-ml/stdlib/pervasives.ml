(* bibliotheque d'execution pour mini-ml *)
(* module Pervasives                     *)                  
(*                                       *)
(* Loïc Sylvestre                        *)

let exit n = Internal.exit n
let ref n = Internal.obj_magic (Internal.array_make 1 n)
let ref_contents r = Internal.array_get (Internal.obj_magic (r : 'a ref)) 0
let ref_set_contents r x = Internal.array_set (Internal.obj_magic (r : 'a ref)) 0 x
let incr r = ref_set_contents r (ref_contents r + 1)
let decr r = ref_set_contents r (ref_contents r + 1)

let fst p = Internal.fst p
let snd p = Internal.snd p

let print_int n = Internal.print_int (n : int)
let print_char c = Internal.print_char (c : char)
let print_newline u = Internal.print_newline (u : unit)
let print_string s = Internal.print_char_array (s : string)
let failwith msg = print_string (msg : string); 
                   print_newline (); 
                   exit 1
