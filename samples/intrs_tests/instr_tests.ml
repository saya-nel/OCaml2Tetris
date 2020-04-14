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
]
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

(* 79 VECTLENGTH *)
(* Resultat attendu : acc = 1 *)
let code = [|103;42;63;0;79|]

(* 80 GETVECTITEM *)
(* Resultat attendu : acc = 42 *)
let code = [|103;0;9;103;42;63;0;80|]

(* 81 SETVECTITEM *)
(* Resultat attendu : acc = <42> *)
let code = [|103;42;9;103;0;9;103;0;63;0;57;0;53;0;81;53;0|]

(* 82 GETSTRINGCHAR *)

(* 83 SETSTRINGCHAR *)

(* 84 BRANCH *)
(* Resultat attendu : pc = 7 *)
let code = [|84;5|]

(* 85 BRANCHIF *)
(* Resultat attendu : pc = 7 *)
let code = [|103;1;85;3|]

(* 86 BRANCHIFNOT *)
(* Resultat attendu : pc = 7 *)
let code = [|103;0;86;3|]

(* 87 SWITCH *)

(* 88 BOOLNOT *)
(* Resultat attendu : acc = 1 *)
let code = [|88|]

(* 89 PUSHTRAP *)

(* 90 POPTRAP *)

(* 91 RAISE *)

(* 92 CHECK-SIGNALS *)

(* 93 C-CALL1 *)

(* 94 C-CALL2 *)

(* 95 C-CALL3 *)

(* 96 C-CALL4 *)

(* 97 C-CALL5 *)

(* 98 C-CALLN *)

(* 99 CONST0 *)
(* Resultat attendu : acc = 0 *)
let code = [|103;1;99|]

(* 100 CONST1 *)
(* Resultat attendu : acc = 1 *)
let code = [|100|]

(* 101 CONST2 *)
(* Resultat attendu : acc = 2 *)
let code = [|101|]

(* 102 CONST3 *)
(* Resultat attendu : acc = 3 *)
let code = [|102|]

(* 103 CONSTINT *)
(* Resultat attendu : acc = 42 *)
let code = [|103;42|]

(* 104 PUSHCONST0 *)
(* Resultat attendu : acc = 0, stack = 42 *)
let code = [|103;42;104|]

(* 105 PUSHCONST1 *)
(* Resultat attendu : acc = 1, stack = 42 *)
let code = [|103;42;105|]

(* 106 PUSHCONST2 *)
(* Resultat attendu : acc = 2, stack = 42 *)
let code = [|103;42;106|]

(* 107 PUSHCONST3 *)
(* Resultat attendu : acc = 3, stack = 42 *)
let code = [|103;42;107|]

(* 108 PUSHCONSTINT *)
(* Resultat attendu : acc = 43, stack = 42 *)
let code = [|103;42;108;43|]

(* 109 NEGINT *)
(* Resultat attendu : acc = 42 *)
let code = [|103;-42;109|]

(* 110 ADDINT *)
(* Resultat attendu : acc = 42 *)
let code = [|103;2;9;103;40;110|]

(* 111 SUBINT *)
(* Resultat attendu : acc = 42 *)
let code = [|103;8;9;103;50;111|]

(* 112 MULINT *)
(* Resultat attendu : acc = 42 *)
let code = [|103;7;9;103;6;112|]

(* 113 DIVINT *)
(* Resultat attendu : acc = 42 *)
let code = [|103;2;9;103;84;113|]

(* 114 MODINT *)
(* Resultat attendu : acc = 2 *)
let code = [|103;3;9;103;8;114|]

(* 115 ANDINT *)
(* Resultat attendu : acc = 1 *)
let code = [|103;1;9;103;1;115|]

(* 116 ORINT *)
(* Resultat attendu : acc = 1 *)
let code = [|103;1;9;103;0;116|]

(* 117 XORINT *)
(* Resultat attendu : acc = 1 *)
let code = [|103;1;9;103;0;117|]

(* 118 LSINT *)
(* Resultat attendu : acc = 60 *)
let code = [|103;2;9;103;15;118|]

(* 119 LSRINT *)
(* Resultat attendu : acc = 3 *)
let code = [|103;2;9;103;15;119|]

(* 120 ASRINT *)
(* Resultat attendu : acc = 3 *)
let code = [|103;2;9;103;15;120|]

(* 121 EQ *)
(* Resultat attendu : acc = 1 *)
let code = [|103;42;9;103;42;121|]

(* 122 NEQ *)
(* Resultat attendu : acc = 1 *)
let code = [|103;42;9;103;43;122|]

(* 123 LTINT *)
(* Resultat attendu : acc = 1 *)
let code = [|103;43;9;103;42;123|]

(* 124 LEINT *)
(* Resultat attendu : acc = 1 *)
let code = [|103;43;9;103;43;124|]

(* 125 GTINT *)
(* Resultat attendu : acc = 1 *)
let code = [|103;42;9;103;43;125|]

(* 126 GEINT *)
(* Resultat attendu : acc = 1 *)
let code = [|103;42;9;103;42;126|]

(* 127 OFFSETINT *)
(* Resultat attendu : acc = 42 *)
let code = [|103;40;127;2|]

(* 128 OFFSETREF *)
(* Resultat attendu : acc = <42> *)
let code = [|103;40;63;0;57;0;53;0;128;2;53;0|]

(* 129 ISINT *)
(* Resulat attendu : acc = 1 *)
let code = [|103;42;129|]

(* 130 GETMETHOD *)
(* Resultat attendu : acc = 42 *)
let code = [|103;42;63;0;63;0;9;103;0;130|]

(* 131 BEQ *)
(* Resultat attendu : pc = 5 *)
let code = [|131;0;3|]

(* 132 BNEQ *)
(* Resultat attendu : pc = 5 *)
let code = [|132;1;3|]

(* 133 BLTINT *)
(* Resultat attendu : pc = 5 *)
let code = [|133;-1;3|]

(* 134 BLEINT *)
(* Resultat attendu : pc = 5 *)
let code = [|134;0;3|]

(* 135 BGTINT *)
(* Resultat attendu : pc = 5 *)
let code = [|135;1;3|]

(* 136 BGEINT *)
(* Resultat attendu : pc = 5 *)
let code = [|136;0;3|]

(* 137 ULTINT *)
(* Resultat attendu : acc = 1 *)
let code = [|103;42;9;103;41;137|]

(* 138 UGEINT *)
(* Resultat attendu : acc = 1 *)
let code = [|103;42;9;103;42;138|]

(* 139 BULTINT *)
(* Resultat attendu : pc = 7 *)
let code = [|103;42;139;41;3|]

(* 140 BUGEINT *)
(* Resultat attendu : pc = 7 *)
let code = [|103;42;140;43;3|]

(* 141 GETPUBMET *)

(* 142 GETDYNMET *)

(* 143 STOP *)
(* pas de test nécéssaire *)

(* 144 EVENT *)

(* 145 BREAK *)

