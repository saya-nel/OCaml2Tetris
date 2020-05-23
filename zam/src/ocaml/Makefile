CC=ocamlc -I ../ -I ../../
EXE=zam
MLVALUES=mlvalues.ml

all:
	$(CC) -c $(MLVALUES)
	$(CC) -c ../prims.ml
	$(CC) -c ../domain.ml
	$(CC) -c ../block.ml
	$(CC) -c ../data.ml
	$(CC) -c ../gc.ml
	$(CC) -c ../alloc.ml
	$(CC) -c ../call.ml
	$(CC) -c ../interp.ml
	$(CC) -c ../../input.ml
	$(CC) -c ../main.ml
	$(CC) -o $(EXE) $(MLVALUES:.ml=.cmo) ../prims.cmo ../domain.cmo ../block.cmo \
	../data.cmo ../gc.cmo ../alloc.cmo  ../call.cmo ../../input.cmo ../interp.cmo  \
	../main.cmo
clean:
	rm -f $(EXE) ../*.cm[oi]