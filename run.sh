#!/bin/bash

if [ $# -eq 0 ]
  then # Si il n'y a pas d'arg, on lance la vm
    # lancement compilation vers mini-ml
    cd mini-ml
    mkdir generated_files
    ./compile stdlib/pervasives.ml stdlib/array.ml stdlib/string.ml ../zam/mlvalues.ml ../zam/prims.ml ../zam/input.ml ../zam/interp.ml
    cd .. 

    # ouverture de la VM nand2tetris
    nand2tetris/tools/VMEmulator.sh
  else
    # clean des anciens résidus 
    rm -Rf mini-ml/generated_files
    rm zam/input.ml

    # lancement make mini-ml

    make -C mini-ml

    # lancement obytelibParser
    dune build bytecode/obytelibParser.exe
    cd samples
    ocamlc *.ml
    rm a.out
    rm *.cmi
    cd ..
    ./_build/default/bytecode/obytelibParser.exe $@

    # lancement compilation vers mini-ml
    cd mini-ml
    mkdir generated_files
    ./compile stdlib/pervasives.ml stdlib/array.ml stdlib/string.ml ../zam/mlvalues.ml ../zam/prims.ml ../zam/input.ml ../zam/interp.ml
    cd .. 

    # ouverture de la VM nand2tetris
    nand2tetris/tools/VMEmulator.sh
fi

