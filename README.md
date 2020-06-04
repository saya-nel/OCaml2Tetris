# OCaml2Tetris

Implémentation de la zam OCaml pour la plateforme Nand2Tetris.

## Description

Une description complète du projet est décrite dans le fichier rapport.pdf.

## Installation

Opam doit être installé. Voici les dépendances :

```bash
$ opam switch create ocaml-base-compiler.4.07.1
$ opam install dune
$ opam install obytelib
$ opam install ocamlclean
```

- NB: obytelib 1.5 nécessite ocaml >= 4.07, mais manipule du bytecode ocaml <= 4.07.1

### Compilation de la ZAM

Dans tous les exemples, nom_du_fichier.ml est aremplacé par le nom du fichier que vous voulez tester dans le dossier bench.

- `make zam-miniML MLFILES=benchs/nom_du_fichier.ml` compile en mini-ml notre implémentation de la ZAM. Les executables sont dans 'zam/bin', accompagné d'un script 'Main.tst' permettant de configurer le simulateur Nand2Tetris. Pour lancer le programme depuis le simulateur, on ouvrira Main.tst avec *file -> load script*, on poura enlever les animations pour rendre la VM plus rapide : *animate -> no animation*, puis on cliquera sur run (bouton double flèche bleue).

- `make zam-ocaml MLFILES=benchs/nom_du_fichier.ml` compile en OCaml notre implémentation de la ZAM. L'executable est 'zam/src/ocaml/zam.exe'

- `make zam-miniML-run MLFILES=benchs/nom_du_fichier.ml` compile en mini-ml notre implémentation de la ZAM, puis lance le simulateur

- `make zam-ocaml-run MLFILES=benchs/nom_du_fichier.ml` compile en OCaml notre implémentation de la ZAM, puis lance l'executable 'zam/src/ocaml/zam.exe'

Pour lancer un programme séparé en plusieurs fichiers, toutes les commandes précédentes peuvent contenir plusieurs noms de fichiers.

Exemple :

```bash
$ make zam-ocaml-run MLFILES="benchs/nom_du_fichier1.ml benchs/nom_du_fichier2.ml"
```

Exemple concret d'utilisation :

```bash
$ make zam-miniML-run MLFILES=benchs/fact.ml
```

### Options pour lancement en miniML

Les options suivantes sont disponibles uniquement si la zam est lancée en miniML : 

- -printpast  : affiche l'AST en syntaxe Caml
- -printast  : affiche l'AST simplifié en syntaxe Caml (après typage et optimisation)
- -typecheck  : type le programme est abandonne si celui-ci est mal typé
- -inline  : profondeur d'inlining
- -noglobalize  : désactive la globalisation des valeurs immutables allouées.
- -nofolding  : désactive la propagation des constantes
- -nosmpvar  : désactive la réécriture des variables globales de la forme [let x = constante] en fonction d'arité 0
- -src  : spécifie où chercher les fichiers sources à compiler
- -dst  : spécifie le dossier où seront placés les fichiers compilés
- -stdlib chemin vers la bibliothèque d'execution de mini-ml
- -assert  : embarque les assertions dans le code.
- -help  Display this list of options
- --help  Display this list of options

Exemple : 

```bash
make zam-miniML-run MLFILES="benchs/fact.ml" MINIML-FLAGS="-printast"
```

### Etapes intermédiaires

- `make miniML` compile (en OCaml) le compilateur `mini-ml`.
- `make link MLFILES="f1.ml f2.ml ..."` compile les sources avec ocamlc. édition des liens avec un fichier primitives.c qui définit l'équivalent OCaml des primitives C supportées par notre implémentation de la ZAM.
- `make ocamlclean` produit l'executable `vm/link/byte.out` à partir de l'executable `vm/link/a.out`
- `make obytelib` construit le tableau code `zam/input.ml` à partir de l'executable `vm/link/byte.out`. 

### Tests unitaires 

- `make test` (seulement pour notre implémentation de la ZAM en OCaml). 

### Organisation des sources
```text
.
├── Makefile
├── vm
│   ├── zam-miniML       # implantation de la ZAM en miniML
│   │   ├── bin
|   |   |   ├── Main.tst ;; exécutable
│   │   │   └── *.vm  
│   │   ├── input.ml
│   │   └── src
│   │       ├── mlvalues.ml  
│   │       ├── domain.ml 
│   │       ├── block.ml 
│   │       ├── data.ml   
│   │       ├── alloc.ml    
│   │       ├── gc.ml  
│   │       ├── prims.ml  
│   │       ├── call.ml  
│   │       ├── interp.ml  
│   │       └── main.ml  
│   ├── zam-OCaml         # implantation de la ZAM en OCaml
│   │   ├── zam.exe      ;; exécutable
│   │   ├── mlvalues.ml
│   │   └── Makefile     ;; le code source est identique à zam-miniML, excepté mlvalues.ml
│   ├── stdlib
│   │   ├── pervasives.ml
│   │   ├── array.ml
│   │   └── list.ml
│   ├── bytecode          # préparation du bytecode
│   │   └── obytelibParser.ml
│   └── link              # production d'un executable nettoyé par ocamlclean
│       ├── primitives.c
│       └── Makefile
├── mini-ml              # implantation en OCaml d'un compilateur (miniML -> Nand2Tetris)
│   ├── README.md
│   ├── generated_files
│   │   └── *.vm 
│   ├── stdlib
│   │   ├── array.ml
│   │   ├── list.ml
│   │   ├── Main.vm
│   │   ├── ML_Internal.vm
│   │   ├── pervasives.ml
│   │   └── string.ml
│   ├── Makefile
│   ├── parseutils.ml
│   ├── lexer.mll
│   ├── lexer.ml
│   ├── parser.mli
│   ├── parser.ml
│   ├── parser.mly
│   ├── past.ml
│   ├── ast.ml
│   ├── kast.ml
│   ├── bc.ml
│   ├── past_print.ml
│   ├── ast_print.ml
│   ├── bc_print.ml
│   ├── past2ast.ml
│   ├── ast2kast.ml
│   ├── kast2bc.ml
│   ├── types.ml
│   ├── typing.ml
│   ├── runtime.ml
│   ├── main.ml
│   ├── gensym.ml
│   ├── freevr.ml
│   ├── bc_fold.ml
│   ├── ast_tailrec.ml
│   ├── ast_smpvar.ml
│   ├── ast_inline.ml
│   ├── ast_lift.ml
│   ├── ast_fold.ml
│   ├── ast_globz.ml
│   └── ast_closure.ml
└── annexes
    ├── micro-ml    # implantation en OCaml d'un compilateur ([λ-calcul + fix] -> Nand2Tetris)
    └── vm-emulator # implantation en OCaml d'un simulateur Nand2Tetris
```
