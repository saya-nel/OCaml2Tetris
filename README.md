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

## Alternative avec *Make*

### compilation de la ZAM
- `make zam-miniML MLFILES=samples/rec/fact.ml` compile en mini-ml notre implémentation de la ZAM. Les executables sont dans `zam/bin`

- `make zam-ocaml MLFILES=samples/rec/fact.ml` compile en OCaml notre implémentation de la ZAM. L'executable est `zam/src/ocaml/zam`

- `make zam-miniML-run MLFILES=samples/rec/fact.ml` compile en mini-ml notre implémentation de la ZAM, puis lance le simulateur

- `make zam-ocaml-run MLFILES=samples/rec/fact.ml` compile en OCaml notre implémentation de la ZAM, puis lance l'executable `zam/src/ocaml/zam`

### étapes intermédiaires

- `make miniML` compile (en OCaml) le compilateur `mini-ml`.
- `make cmo MLFILES="f1.ml f2.ml ..."` compile les sources avec ocamlc. édition des liens avec un fichier primitives.c qui définit l'équivalent OCaml des primitives C supportées par notre implémentation de la ZAM.

- `make obytelib MLFILES="f1.cmo f2.cmo ..."` nettoie et fussionne des cmo, puis construit le tableau code `zam/input.ml`.
