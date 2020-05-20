#!/bin/bash

if [ $# -eq 0 ]
  then # pas d'argument , on lance avec input.ml
    make 
    ./zam ../../input.cmo
    rm zam ../*.cmo ../*.cmi
  else # arguments, on doit passer par obytelibParser
    make
    # lancement obytelibParser
    cd ../../..
    dune build bytecode/obytelibParser.exe
    cd samples
    ocamlc *.ml
    rm -f a.out
    rm -f *.cmi
    cd ..
    ./_build/default/bytecode/obytelibParser.exe $@
    # on retourne dans le dossier ocaml et execute zam
    cd zam/src/ocaml
    ./zam ../../input.cmo
    rm zam ../*.cmo ../*.cmi
fi