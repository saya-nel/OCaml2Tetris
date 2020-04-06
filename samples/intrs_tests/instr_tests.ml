(* 
Ce fichier est un fichier de test pour tester chaque instructions
de la zam, à chaque instruction correspond un tableau de bytecode
interpretable par la zam, il faut donc copier le tableau de
l'instruction qu'on veut tester dans la fichier : input.ml.
Ensuite, il faudra lancer la zam normalement avec compile, 
puis vérifier que le resultat attendu est le même que celui
donné par la vm nand2tetris.
*)

(* ACC0 *)
(* Resultat attendu : acc = 42 *)
let code = [|103;42;9;0|]

(* ACC1 *)
(* Resultat attendu : acc = 42 *)
let code = [|103;42;9;103;1;9;1|]

(* ACC2 *)
(* Resultat attendu : acc = 42 *)
let code = [|103;42;9;103;1;9;103;1;9;2|]

(* ACC3 *)
(* Resultat attendu : acc = 42 *)
let code = [|103;42;9;103;1;9;103;1;9;103;1;9;3|]

(* ACC4 *)
(* Resultat attendu : acc = 42 *)
let code = [|103;42;9;103;1;9;103;1;9;103;1;9;103;1;9;4|]

(* ACC5 *)
(* Resultat attendu : acc = 42 *)
let code = [|103;42;9;103;1;9;103;1;9;103;1;9;103;1;9;103;1;9;5|]

(* ACC6 *)
(* Resultat attendu : acc = 42 *)
let code = [|103;42;9;103;1;9;103;1;9;103;1;9;103;1;9;103;1;9;103;1;9;6|]

(* ACC7 *)
(* Resultat attendu : acc = 42 *)
let code = [|103;42;9;103;1;9;103;1;9;103;1;9;103;1;9;103;1;9;103;1;9;103;1;9;7|]

(* ACC *)
(* Resultat attendu : acc = 42 *)
let code = [|103;42;9;103;1;9;103;1;9;103;1;9;8;3|]

(* PUSH *)
(* Resultat attendu : acc = 42 *)
let code = [|103;42;9;0|]

(* PUSHACC0 *)
(* Resultat attendu : acc = 42, sommet de pile = 42 *)
let code = [|103;42;10|]

(* PUSHACC1 *)
(* Resultat attendu : acc = 42, sommet de pile = 2 *)
let code = [|103;42;9;103;2;11|]

(* PUSHACC2 *)
(* Resultat attendu : acc = 42, sommet de pile = 2 *)
let code = [|103;42;9;103;1;9;103;2;12|]

(* PUSHACC3 *)
(* Resultat attendu : acc = 42, sommet de pile = 2 *)
let code = [|103;42;9;103;1;9;103;1;9;103;2;13|]

(* PUSHACC4 *)
(* Resultat attendu : acc = 42, sommet de pile = 2 *)
let code = [|103;42;9;103;1;9;103;1;9;103;1;9;103;2;14|]

(* PUSHACC5 *)
(* Resultat attendu : acc = 42, sommet de pile = 2 *)
let code = [|103;42;9;103;1;9;103;1;9;103;1;9;103;1;9;103;2;15|]

(* PUSHACC6 *)
(* Resultat attendu : acc = 42, sommet de pile = 2 *)
let code = [|103;42;9;103;1;9;103;1;9;103;1;9;103;1;9;103;1;9;103;2;16|]

(* PUSHACC7 *)
(* Resultat attendu : acc = 42, sommet de pile = 2 *)
let code = [|103;42;9;103;1;9;103;1;9;103;1;9;103;1;9;103;1;9;103;1;9;103;2;17|]

(* PUSHACC *)
(* Resultat attendu : acc = 42, sommet de pile = 2 *)
let code = [|103;42;9;103;1;9;103;1;9;103;2;18;3|]

(* POP *)
(* Resultat attendu : acc = 1, stack = 42 *)
let code = [|103;42;9;103;1;9;103;1;9;19;2|]

(* ASSIGN *)
(* Resultat attendu : acc = 0, stack = 42 | 1 *)
let code = [|103;1;9;103;1;9;103;42;20;1|]