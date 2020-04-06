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
