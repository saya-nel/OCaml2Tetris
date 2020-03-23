#!/bin/bash

# lancement make mini-ml
cd mini-ml
make
cd ..

# lancement obytelibParser
dune build src/obytelibParser.exe
cd samples
ocamlc *.ml
rm a.out
rm *.cmi
../_build/default/src/obytelibParser.exe $@
cd .. 

# lancement compilation vers mini-ml
cd mini-ml
mkdir generated_files
./compile ../src/zam/mlvalues.ml ../src/zam/prims.ml ../src/zam/interp.ml
cd ..