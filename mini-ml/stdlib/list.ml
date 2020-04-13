(* bibliotheque d'exécution pour mini-ml *)
(* module List                           *)                  
(*                                       *)
(* Loïc Sylvestre                        *)

let cons (x : 'a) (l : 'a list) : 'a list = Internal.cons x l
let hd (l : 'a list) : 'a = Internal.hd l
let tl (l : 'a list) : 'a list = Internal.tl l

let rec map f l =
   match l with
   | [] -> []
   | _ ->  (f (hd l)) :: (map f (tl l))

