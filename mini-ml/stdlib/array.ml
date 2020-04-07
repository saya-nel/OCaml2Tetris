(* bibliotheque d'execution pour mini-ml *)
(* module Array                          *)                  
(*                                       *)
(* Loïc Sylvestre                        *)

let make n x = Internal.array_make n x
let create_uninitialized n = Internal.array_create_uninitialized n
let get a i = Internal.array_get a i
let set a i x = Internal.array_set a i x
let length a = Internal.array_length a

let iter f a =
  for i = 0 to length a - 1 do f (get a i) done 

let map f a = 
	let n = length a in
	if n = 0 then [||] 
    else let a2 = make n (get a 0) in
           for i = 0 to n - 1 do
   	 set a2 i (f (get a i))
  done;
  a2
