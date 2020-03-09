#!/bin/bash

# Dinstr launch
# dune build src/bytecodeParser.exe 
# cd samples
# ocamlc -dinstr $@ &> res.txt 
# cat res.txt | ../_build/default/src/bytecodeParser.exe

# Obytelib launch
dune build src/obytelibParser.exe
cd samples
../_build/default/src/obytelibParser.exe $@

cd .. 
# Clean folders
# dune clean 
# rm samples/*.cmi 
# rm samples/a.out
# rm samples/res.txt
