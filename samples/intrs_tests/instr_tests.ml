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

(* 21 ENVACC1 *)

(* 22 ENVACC2 *)

(* 23 ENVACC3 *)

(* 24 ENVACC4 *)

(* 25 ENVACC *)

(* 26 PUSHENVACC1 *)

(* 27 PUSHENVACC2 *)

(* 28 PUSHENVACC3 *)

(* 29 PUSHENVACC4 *)

(* 30 PUSHENVACC *)

(* 31 PUSHRETADDR *)

(* 32 APPLY *)

(* 33 APPLY1 *)

(* 34 APPLY2 *)

(* 35 APPLY3 *)

(* 36 APPTERM *)

(* 37 APPTERM1 *)

(* 38 APPTER2 *)

(* 39 APPTERM3 *)

(* 40 RETURN *)

(* 41 RESTART *)

(* 42 GRAB *)

(* 43 CLOSURE *)

(* 44 CLOSUREREC *)

(* 45 OFFSETCLOSUREM2 *)

(* 46 OFFSETCLOSURE0 *)

(* 47 OFFSETCLOSURE2 *)

(* 48 OFFSETCLOSURE *)

(* 49 PUSHOFFSETCLOSUREM2 *)

(* 50 PUSHOFFSETCLOSURE0 *)

(* 51 PUSHOFFSETCLOSURE2 *)

(* 52 PUSHOFFSETCLOSURE *)

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
(* Resultat attendu : acc = (tag=1) <42> | <43> *)
let code = [|103;43;9;103;42;62;2;1|]

(* 63 MAKEBLOCK1 *)
(* Resultat attendu : acc = (tag=1) <42> *)
let code = [|103;42;63;1|]

(* 64 MAKEBLOCK2 *)
(* Resultat attendu : acc = (tag=1) <42> | <43> *)
let code = [|103;43;9;103;42;64;1|]

(* 65 MAKEBLOCK3 *)
(* Resultat attendu : acc = (tag=1) <42> | <43> | <44>*)
let code = [|103;44;9;103;43;9;103;42;65;1|]

(* 67 GETFIELD0 *)
(* Resultat attendu : acc = 42 *)
let code = [|103;42;63;1;67|]

(* 68 GETFIELD1 *)
(* Resultat attendu : acc = 43 *)
let code = [|103;43;9;103;42;64;1;68|]

(* 69 GETFIELD2 *)
(* Resultat attendu : acc = 44 *)
let code = [|103;44;9;103;43;9;103;42;64;1;69|]

(* 70 GETFIEL3 *)
(* Resultat attendu : acc = 45 *)
let code = [|103;45;9;103;44;9;103;43;9;103;42;62;4;1;70|]

(* 71 GETFIELD *)
(* Resultat attendu : acc = 42 *)
let code = [|103;42;63;1;71;0|]

(* 72 GETFLOATFIELD *)

(* 73 SETFIELD0 *)
(* Resultat attendu : acc = <1> *)
let code = [|103;1;9;103;42;62;1;1;57;0;53;0;73;53;0|]

(* 74 SETFIELD1 *)
(* Resultat attendu : acc = <42> | <1> *)
let code = [|103;1;9;103;43;9;103;42;62;2;1;57;0;53;0;74;53;0|]

(* 75 SETFIELD2 *)
(* Resultat attendu : acc = <42> | <43> | <1> *)
let code = [|103;1;9;103;44;9;103;43;9;103;42;62;3;1;57;0;53;0;75;53;0|]

(* 76 SETFIELD3 *)
(* Resultat attendu : acc = <42> | <43> | <44> | <1> *)
let code = [|103;1;9;103;45;9;103;44;9;103;43;9;103;42;62;4;1;57;0;53;0;76;53;0|]

(* 77 SETFIELD *)
(* Resultat attendu : acc = <1> *)
let code = [|103;1;9;103;42;62;1;1;57;0;53;0;77;0;53;0|]

(* 78 SETFLOATFIELD *)