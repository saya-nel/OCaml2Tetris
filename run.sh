dune build src/bytecodeParser.exe && ocamldumpobj ./samples/$1 | ./_build/default/src/bytecodeParser.exe && dune clean
