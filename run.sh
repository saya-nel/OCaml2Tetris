#!/bin/bash

# Build executable
dune build src/bytecodeParser.exe 
# Run executable with ocamlc -dinstr return
cd samples
ocamlc -dinstr $@ &> res.txt 
cat res.txt | ../_build/default/src/bytecodeParser.exe
cd .. 
# Clean folders
dune clean 
rm samples/*.cmi 
rm samples/*.cmo 
rm samples/a.out
rm samples/res.txt
