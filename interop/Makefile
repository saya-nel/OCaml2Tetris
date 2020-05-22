CAMLC=ocamlc
COMPFLAGS= -i
MLFILES= # foo.ml
CFILES=primitives.c
EXE=a.out


all: n2t.ml $(MLFILES)
	$(CAMLC) -custom -o $(EXE) $(CFILES) n2t.ml $(MLFILES) 

bytecode:	clean-all all clean

	# 1) néttoie les anciens résidu # 2) compile (avec interop) # 3) néttoie tout sauf les .cmo

.SUFFIXES: .c .o .ml .mli .cmo .cmi .cmx

.c.o:
	cc -c -I`ocamlc -where` $<
.ml.cmo:
	$(CAMLC)$(FLAGS) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(FLAGS) $(COMPFLAGS) -c $<

clean:
	rm -rf  *~ *.cm[ix] *.o $(EXE)

clean-cmo:
	rm -rf *.cmo

clean-all:	clean clean-cmo
