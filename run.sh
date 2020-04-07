#!/bin/bash

VMEMULATOR="nand2tetris/tools/VMEmulator.sh"

ZAM_BIN="zam/bin/"

MINI_ML="mini-ml"

# chemins relatif depuis mini-ml
ROOT="../"
STDLIB="stdlib/pervasives.ml stdlib/array.ml stdlib/string.ml"
ZAM_INPUT="../zam/input.ml"
ZAM_SRC="../zam/src/mlvalues.ml ../zam/src/prims.ml ../zam/src/check.ml \
         ../zam/src/interp.ml ../zam/src/main.ml"

if [ $# -eq 0 ]
  then # Si il n'y a pas d'arg, on lance la vm
    # lancement compilation vers mini-ml
    mkdir -p $ZAM_BIN
    cd $MINI_ML
    make
    ./compile -dst=$ROOT$ZAM_BIN $STDLIB $ZAM_INPUT $ZAM_SRC
    cd $ROOT 

    # ouverture de la VM nand2tetris
    $VMEMULATOR

  else
    
    # clean des anciens résidus 
    rm -f $ZAM_BIN/*
    rm -f $ZAM_INPUT

    # lancement make mini-ml

    make -C $MINI_ML

    # lancement obytelibParser
    dune build bytecode/obytelibParser.exe
    cd samples
    ocamlc *.ml
    rm -f a.out
    rm -f *.cmi
    cd ..
    ./_build/default/bytecode/obytelibParser.exe $@

    # lancement compilation vers mini-ml
    mkdir -p $ZAM_BIN
    cd $MINI_ML
    ./compile -dst=../$ZAM_BIN $STDLIB $ZAM_INPUT $ZAM_SRC
    cd $ROOT

    # ouverture de la VM nand2tetris
    $VMEMULATOR
fi

