(* bibliotheque d'execution pour mini-ml *)
(* module Array                          *)                  
(*                                       *)
(* Loïc Sylvestre                        *)

let make n c = 
	( (Internal.obj_magic
	   (Internal.array_make n (c : char))) : string )

let get s i = 
	( (Internal.array_get (Internal.obj_magic (s : string)) i) : char)

let length s = 
	(Internal.array_length (Internal.obj_magic (s : string)))
