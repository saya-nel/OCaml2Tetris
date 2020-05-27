#!/bin/bash

if [[ $# -eq 0 && -f "vm/zam-OCaml/zam.exe" ]]
  then # pas d'argument , on lance avec input.ml 
    ./vm/zam-OCaml/zam.exe
  else # arguments, on doit passer par obytelibParser
    # lancement obytelibParser
    dune build vm/bytecode/obytelibParser.exe
    cd samples
    ocamlc *.ml
    rm -f a.out
    rm -f *.cmi
    cd ..
    ./_build/default/vm/bytecode/obytelibParser.exe $@
    # on retourne dans le dossier ocaml et execute zam
    make -C vm/zam-OCaml
    vm/zam-OCaml/zam.exe
    ### make clean -C vm/zam-OCaml
fi