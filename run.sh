dune build src/bytecodeParser.exe && ocamlc -dinstr ./samples/$1 | ./_build/default/src/bytecodeParser.exe && dune clean && rm samples/*.cmi && rm samples/*.cmo && rm a.out

