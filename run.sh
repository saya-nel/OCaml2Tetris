#!/bin/bash

# lancement make mini-ml

make -C mini-ml

# lancement obytelibParser
dune build bytecode/obytelibParser.exe
cd samples
ocamlc *.ml
rm a.out
rm *.cmi
../_build/default/bytecode/obytelibParser.exe $@
cd .. 

# lancement compilation vers mini-ml
cd mini-ml
mkdir generated_files
./compile stdlib/pervasives.ml stdlib/array.ml stdlib/string.ml ../zam/mlvalues.ml ../zam/prims.ml ../zam/input.ml ../zam/interp.ml
cd .. 

# ouverture de la VM nand2tetris
nand2tetris/tools/VMEmulator.sh