if [ $# -eq 0 ]
  then # pas d'argument , on lance avec input.ml
    make 
    ./zam ../../input.cmo
    rm zam ../alloc.cmo ../mlvalues.cmo ../prims.cmo ../interp.cmo ../../input.cmo ../main.cmo mlvalues.cmi prims.cmi ../interp.cmi ../../input.cmi ../main.cmi
  else # arguments, on doit passer par obytelibParser
    make
    # lancement obytelibParser
    cd ../../..
    dune build bytecode/obytelibParser.exe
    cd samples
    ocamlc *.ml
    rm -f a.out
    rm -f *.cmi
    cd ..
    ./_build/default/bytecode/obytelibParser.exe $@
    # on retourne dans le dossier ocaml et execute zam
    cd zam/src/ocaml
    ./zam ../../input.cmo
    rm zam ../aloc.cmo ../mlvalues.cmo ../prims.cmo ../interp.cmo ../../input.cmo ../main.cmo mlvalues.cmi prims.cmi ../interp.cmi ../../input.cmi ../main.cmi
fi