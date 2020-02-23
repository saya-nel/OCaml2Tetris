# Références utiles :
- pdf jack : https://www.csie.ntu.edu.tw/~cyy/courses/introCS/19fall/lectures/handouts/lec13_Jack.pdf
- lib standard jack : https://classes.engineering.wustl.edu/cse365/jack.php

# Semaine 1 :

## Réfléchir representation mémoire :
tas : tableau mais de quoi ? (possible : int avec 1 bit pour type direct ou pointeur, classe avec int à l'interrieur).

## Exemples d'interprètes :
Omicrobe, ocamlrun

## A faire :
- regarder fonctionnement nand2tetris
- mettre en place environement de travail
- regarder representation mémoire (travail macros / fonctions)
- créer depot git



# Semaine 2 : 

## Pour le rapport :
exposer la plateforme Nand2Tetris -> Objectif changer de langage -> gc -> biblio d'exec

## A faire : 
- regarder interprete (Omicrob, zankov)
- structuration interprete
- representation des valeurs
- commencer à faire parser (voir découpage fichiers code octet)
- créer le fichier de compte rendu hebdomadaire



# Semaine 3 :

- Obytelib pour tranformer en Ocaml tu bytode Ocaml en tableau jack contenant le bytecode dans un fichier .jack (Exemple Omicrobe disponible).
- Ocaml clean pour nettoyer le bytecode 
- Chaque instruction de la VM est associé a une classe Jack, contenant une méthode apply. 

# Semaine 4 :

- Obytelib pour tranformer en Ocaml du bytecode Ocaml en programme miniml (a la fin : un seul fichier representant la vm (interpretation bytecode + gc), on peut faire des cat et tout rassembler dans un fichier).
- voir ocamldumpobj test1.cmo
- Continuer de transformer miniml en .vm Nand2Tetris. 

- notes pour GC : 
  - racines : registre et pile + variables globales.

