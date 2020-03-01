dune build src/bytecodeParser.exe && cd samples && ocamlc -dinstr $@ | ../_build/default/src/bytecodeParser.exe && cd .. && dune clean && rm samples/*.cmi && rm samples/*.cmo && rm samples/a.out

