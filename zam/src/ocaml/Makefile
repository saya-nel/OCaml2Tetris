CC=ocamlc -I ../ -I ../../
EXE=zam

all:
	$(CC) -c ../mlvalues.ml
	$(CC) -c ../prims.ml
	$(CC) -c ../domain.ml
	$(CC) -c ../block.ml
	$(CC) -c ../gc.ml
	$(CC) -c ../alloc.ml
	$(CC) -c ../call.ml
	$(CC) -c ../interp.ml
	$(CC) -c ../../input.ml
	$(CC) -c ../main.ml
	$(CC) -o $(EXE) ../mlvalues.cmo ../prims.cmo ../domain.cmo ../block.cmo \
	../gc.cmo ../alloc.cmo  ../call.cmo ../interp.cmo ../../input.cmo \
	../main.cmo
clean:
	rm -f $(EXE) ../*.cm[oi]