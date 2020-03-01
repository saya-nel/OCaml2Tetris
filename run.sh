# Build executable
dune build src/bytecodeParser.exe 
# Run executable with ocamlc -dinstr return
cd samples
ocamlc -dinstr $@ | ../_build/default/src/bytecodeParser.exe 
cd .. 
# Clean folders
dune clean 
rm samples/*.cmi 
rm samples/*.cmo 
rm samples/a.out

