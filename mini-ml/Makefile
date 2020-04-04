CC=ocamlc
EXE=compile

all:
	$(CC) -c parseutils.ml
	ocamllex lexer.mll       # generates lexer.ml
	ocamlyacc parser.mly     # generates parser.ml and parser.mli
	$(CC) -c ast.ml
	$(CC) -c print_ast.ml
	$(CC) -c kast.ml
	$(CC) -c types.ml
	$(CC) -c typing.ml
	$(CC) -c runtime.ml
	$(CC) -c  ast2kast.ml
	$(CC) -c  bytecode.ml
	$(CC) -c  kast2bytecode.ml
	$(CC) -c bytecode2string.ml
	$(CC) -c parser.mli
	$(CC) -c lexer.ml
	$(CC) -c parser.ml
	$(CC) -c main.ml
	$(CC) -o $(EXE) parseutils.cmo lexer.cmo ast.cmo print_ast.cmo types.cmo typing.cmo kast.cmo ast2kast.cmo bytecode.cmo kast2bytecode.cmo bytecode2string.cmo parser.cmo runtime.cmo main.cmo

clean:
	rm $(EXE) *.cmo *.cmi compile *.vm *.tst *.out