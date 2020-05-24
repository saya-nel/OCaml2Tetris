CAMLC=ocamlc
COMPFLAGS= -i
MLFILES= # foo.ml
CFILES=primitives.c
EXE=a.out
II=$(foreach f,$(MLFILES),-I $(dir $(f)))

all: n2t.ml $(MLFILES)
	$(CAMLC) $(II) -custom -o $(EXE) $(CFILES) n2t.ml $(MLFILES) 

bytecode:	clean-all all clean

	# 1) néttoie les anciens résidu # 2) compile (avec interop) # 3) néttoie tout sauf les .cmo

.SUFFIXES: .c .o .ml .mli .cmo .cmi .cmx

.c.o:
	cc -c -I`ocamlc -where` $<
.ml.cmo:
	$(CAMLC) $(II) $(FLAGS) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(II) $(FLAGS) $(COMPFLAGS) -c $<

clean:
	rm -rf  *~ *.cm[ix] *.o $(EXE)

clean-cmo:
	rm -rf *.cmo

clean-all:	clean clean-cmo
