# OCaml2Tetris

# Lancement

Pour lancer le projet, il faut posseder opam et dune, sur Ubuntu on peut les installer de la manière suivante :

`sudo apt install opam`

`opam install dune`

On peut ensuite lancer le projet, depuis la racine de celui ci, en passant un ou plusieurs fichier .ml, avec l'extension .cmo se trouvant dans /samples en paramètre (si le .cmo n'existe pas il sera généré) :

`./run.sh test1.cmo`
`./run.sh amodule.cmo bmodule.cmo`
