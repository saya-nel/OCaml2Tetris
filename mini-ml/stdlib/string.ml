(* bibliotheque d'exécution pour mini-ml *)
(* module String                         *)                  
(*                                       *)
(* Loïc Sylvestre                        *)

let make (n : int) (c : char) : string = 
  Internal.obj_magic (Internal.array_make n (Internal.obj_magic c))

let get (s : string) (i : int) : char = 
	Internal.array_get (Internal.obj_magic s) i

let length (s : string) : int = 
	Internal.array_length (Internal.obj_magic s)
