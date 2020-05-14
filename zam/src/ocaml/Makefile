CC=ocamlc -I ../ -I ../../
EXE=zam

all:
	$(CC) -c ../alloc.ml
	$(CC) -c ../mlvalues.ml
	$(CC) -c ../prims.ml
	$(CC) -c ../interp.ml
	$(CC) -c ../../input.ml
	$(CC) -c ../main.ml
	$(CC) -o $(EXE) ../alloc.cmo ../mlvalues.cmo ../prims.cmo interp.cmo input.cmo main.cmo
clean:
	rm -f $(EXE) ../alloc.cmo ../mlvalues.cmo ../prims.cmo ../interp.cmo ../../input.cmo ../main.cmo \
	../mlvalues.cmi prims.cmi ../interp.cmi ../../input.cmi ../main.cmi