(* 
Ce fichier est un fichier de test pour tester chaque instructions
de la zam, à chaque instruction correspond un tableau de bytecode
interpretable par la zam, il faut donc copier le tableau de
l'instruction qu'on veut tester dans la fichier : input.ml.
Ensuite, il faudra lancer la zam normalement avec compile, 
puis vérifier que le resultat attendu est le même que celui
donné par la vm nand2tetris.
*)

(* 0 ACC0 *)
(* Resultat attendu : acc = 42 *)
let code = [|103;42;9;0|]

(* 1 ACC1 *)
(* Resultat attendu : acc = 42 *)
let code = [|103;42;9;103;1;9;1|]

(* 2 ACC2 *)
(* Resultat attendu : acc = 42 *)
let code = [|103;42;9;103;1;9;103;1;9;2|]

(* 3 ACC3 *)
(* Resultat attendu : acc = 42 *)
let code = [|103;42;9;103;1;9;103;1;9;103;1;9;3|]

(* 4 ACC4 *)
(* Resultat attendu : acc = 42 *)
let code = [|103;42;9;103;1;9;103;1;9;103;1;9;103;1;9;4|]

(* 5 ACC5 *)
(* Resultat attendu : acc = 42 *)
let code = [|103;42;9;103;1;9;103;1;9;103;1;9;103;1;9;103;1;9;5|]

(* 6 ACC6 *)
(* Resultat attendu : acc = 42 *)
let code = [|103;42;9;103;1;9;103;1;9;103;1;9;103;1;9;103;1;9;103;1;9;6|]

(* 7 ACC7 *)
(* Resultat attendu : acc = 42 *)
let code = [|103;42;9;103;1;9;103;1;9;103;1;9;103;1;9;103;1;9;103;1;9;103;1;9;7|]

(* 8 ACC *)
(* Resultat attendu : acc = 42 *)
let code = [|103;42;9;103;1;9;103;1;9;103;1;9;8;3|]

(* 9 PUSH *)
(* Resultat attendu : acc = 42 *)
let code = [|103;42;9;0|]

(* 10 PUSHACC0 *)
(* Resultat attendu : acc = 42, sommet de pile = 42 *)
let code = [|103;42;10|]

(* 11 PUSHACC1 *)
(* Resultat attendu : acc = 42, sommet de pile = 2 *)
let code = [|103;42;9;103;2;11|]

(* 12 PUSHACC2 *)
(* Resultat attendu : acc = 42, sommet de pile = 2 *)
let code = [|103;42;9;103;1;9;103;2;12|]

(* 13 PUSHACC3 *)
(* Resultat attendu : acc = 42, sommet de pile = 2 *)
let code = [|103;42;9;103;1;9;103;1;9;103;2;13|]

(* 14 PUSHACC4 *)
(* Resultat attendu : acc = 42, sommet de pile = 2 *)
let code = [|103;42;9;103;1;9;103;1;9;103;1;9;103;2;14|]

(* 15 PUSHACC5 *)
(* Resultat attendu : acc = 42, sommet de pile = 2 *)
let code = [|103;42;9;103;1;9;103;1;9;103;1;9;103;1;9;103;2;15|]

(* 16 PUSHACC6 *)
(* Resultat attendu : acc = 42, sommet de pile = 2 *)
let code = [|103;42;9;103;1;9;103;1;9;103;1;9;103;1;9;103;1;9;103;2;16|]

(* 17 PUSHACC7 *)
(* Resultat attendu : acc = 42, sommet de pile = 2 *)
let code = [|103;42;9;103;1;9;103;1;9;103;1;9;103;1;9;103;1;9;103;1;9;103;2;17|]

(* 18 PUSHACC *)
(* Resultat attendu : acc = 42, sommet de pile = 2 *)
let code = [|103;42;9;103;1;9;103;1;9;103;2;18;3|]

(* 19 POP *)
(* Resultat attendu : acc = 1, stack = 42 *)
let code = [|103;42;9;103;1;9;103;1;9;19;2|]

(* 20 ASSIGN *)
(* Resultat attendu : acc = 0, stack = 42 | 1 *)
let code = [|103;1;9;103;1;9;103;42;20;1|]

(* 53 GETGLOBAL *)
(* Resultat attendu : acc = 42 *)
let code = [|103;42;57;1;53;1|]

(* 54 PUSHGETGLOBAL *)
(* Resultat attendu : stack = 1 , acc = 42 *)
let code = [|103;42;57;1;103;1;54;1|]

(* 55 GETGLOBALFIELD *)
(* Resultat attendu : acc = 43 *)
let code = [|103;43;9;103;42;62;2;1;57;1;55;1;1|]

(* 56 PUSHGETGLOBALFIELD *)
(* Resultat attendu : acc = 43, stack = 1 *)
let code = [|103;43;9;103;42;62;2;1;57;1;103;1;56;1;1|]

(* 57 SETGLOBAL *)
(* Resultat attendu : acc = 0, global = 0 | 42 | 0 | 0 ... *)
let code = [|103;42;57;1|]

(* 58 ATOM0 *)
(* Resultat attendu : acc = (block size=0, tag=0) *)
let code = [|58|]

(* 59 ATOM *)
(* Resultat attendu : acc = (block size=0, tag=1) *)
let code = [|59;1|]

(* 60 PUSHATOM0 *)
(* Resultat attendu : acc = (block size=0, tag=0), stack : 42 *)
let code = [|103;42;60|]

(* 61 PUSHATOM *)
(* Resultat attendu : acc = (block size=0, tag=1), stack : 42 *)
let code = [|103;42;61;1|]

(* 62 MAKEBLOCK *)
(* Resultat attendu : acc = <42> | <43> *)
let code = [|103;43;9;103;42;62;2;1|]