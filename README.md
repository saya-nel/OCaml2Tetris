# OCaml2Tetris

# Lancement

Pour lancer le projet, il faut posseder opam et dune, sur Ubuntu on peut les installer de la manière suivante :

`sudo apt install opam`

`opam install dune`

On peut ensuite lancer le projet, depuis la racine de celui ci, en passant un ou plusieurs fichier .ml, avec l'extension .cmo (si le .cmo n'existe pas il sera généré) :

`./run.sh samples/test1.cmo`
`./run.sh samples/amodule.cmo samples/bmodule.cmo`

un fichier "Main.tst" sera généré dans zam/bin, ce fichier devra être interprété par la VM nand2tetris qui s'est lancée à la fin du script.
On ouvrira Main.tst avec **file -> load script**, on poura enlever les animations pour rendre la vm plus rapide : **animate -> no animation**, puis on cliquera sur run (bouton double flèche bleu).

---
## Alternative

## Installation
```bash
$ opam switch create ocaml-base-compiler.4.07.1
$ opam install dune
$ opam install obytelib 
```
-  NB: obytelib 1.5 necessite ocaml >= 4.07, mais manipule du bytecode ocaml <= 4.07.1

### compilation de la ZAM
- `make zam-miniML MLFILES=samples/rec/fact.ml` compile en mini-ml notre implémentation de la ZAM. Les executables sont dans `zam/bin`, accompagné d'un script `Main.tst` permettant de configurer le simulateur Nand2Tetris. Pour lancer le programme depuis le simulateur, on ouvrira Main.tst avec *file -> load script*, on poura enlever les animations pour rendre la vm plus rapide : *animate -> no animation*, puis on cliquera sur run (bouton double flèche bleu).

- `make zam-ocaml MLFILES=samples/rec/fact.ml` compile en OCaml notre implémentation de la ZAM. L'executable est `zam/src/ocaml/zam`

- `make zam-miniML-run MLFILES=samples/rec/fact.ml` compile en mini-ml notre implémentation de la ZAM, puis lance le simulateur

- `make zam-ocaml-run MLFILES=samples/rec/fact.ml` compile en OCaml notre implémentation de la ZAM, puis lance l'executable `zam/src/ocaml/zam`

### étapes intermédiaires

- `make miniML` compile (en OCaml) le compilateur `mini-ml`.
- `make cmo MLFILES="f1.ml f2.ml ..."` compile les sources avec ocamlc. édition des liens avec un fichier primitives.c qui définit l'équivalent OCaml des primitives C supportées par notre implémentation de la ZAM.

- `make obytelib MLFILES="f1.cmo f2.cmo ..."` nettoie et fussionne des cmo, puis construit le tableau code `zam/input.ml`.

### tests unitaires 

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
│   ├── pack              # fusion de plusieurs .cmo en un seul
│   │   └── Makefile
│   └── link              # édition de liens, pour compatibilité avec ocamlrun
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

