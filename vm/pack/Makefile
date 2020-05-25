CAMLC=ocamlc -nopervasives
MLFILES=m1.ml m2.ml m3.ml m4.ml
FLAGS=-open Pervasives
CMO=main.cmo
OBJS= $(subst .ml,.cmo,$(MLFILES))

INCLUDES=-I ../stdlib $(foreach f,$(MLFILES),-I $(dir $(f)))

all: clean-cmo ../stdlib/pervasives.cmo $(OBJS)
	$(CAMLC) $(FLAGS)  $(INCLUDES) -pack $(OBJS) -o $(CMO)

.SUFFIXES: .c .o .ml .mli .cmo .cmi .cmx

.c.o:
	cc -c -I`ocamlc -where` $<

../stdlib/pervasives.cmo:	../stdlib/pervasives.ml
	$(CAMLC) $(INCLUDES) $(INCLUDES) -for-pack $(basename $(notdir $<)) -c $< 

.ml.cmo:
	$(CAMLC) $(INCLUDES) $(FLAGS)  $(INCLUDES) -for-pack $(basename $(notdir $<)) -c $< 

#.mli.cmi:
#	$(CAMLC) $(INCLUDES) $(FLAGS) -c $<

clean:
	rm -rf  *~ *.cm[ix] *.o 

clean-cmo:
	rm -rf *.cmo $(OBJS)

clean-all:	clean clean-cmo
