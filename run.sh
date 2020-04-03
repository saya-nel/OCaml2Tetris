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
./compile ../zam/mlvalues.ml ../zam/prims.ml ../zam/input.ml ../zam/interp.ml
cd ..