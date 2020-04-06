# OCaml2Tetris

# Lancement

Pour lancer le projet, il faut posseder opam et dune, sur Ubuntu on peut les installer de la manière suivante :

`sudo apt install opam`

`opam install dune`

On peut ensuite lancer le projet, depuis la racine de celui ci, en passant un ou plusieurs fichier .ml, avec l'extension .cmo se trouvant dans /samples en paramètre (si le .cmo n'existe pas il sera généré) :

`./run.sh test1.cmo`
`./run.sh amodule.cmo bmodule.cmo`

un fichier "Main.tst" sera généré dans _mini-ml/generated_files_, ce fichier devra être interprété par la VM nand2tetris qui s'est lancée à la fin du script.
On ouvrira Main.tst avec **file -> load script**, on poura enlever les animations pour rendre la vm plus rapide : **animate -> no animation**, puis on cliquera sur run (bouton double flèche bleu).
