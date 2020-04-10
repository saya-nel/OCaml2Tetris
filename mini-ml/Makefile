CC=ocamlc
EXE=compile

all:
	$(CC) -c parseutils.ml
	ocamllex lexer.mll       # generates lexer.ml
	ocamlyacc parser.mly     # generates parser.ml and parser.mli
	$(CC) -c types.ml
	$(CC) -c past.ml
	$(CC) -c past_print.ml
	$(CC) -c ast.ml
	$(CC) -c ast_print.ml
	$(CC) -c typing.ml
	$(CC) -c past2ast.ml
	$(CC) -c ast_inline.ml
	$(CC) -c ast_fold.ml
	$(CC) -c ast_globz.ml
	$(CC) -c freevr.ml
	$(CC) -c ast_lift.ml
	$(CC) -c iast.ml
	$(CC) -c ast2iast.ml
	$(CC) -c iast_tailrec.ml
	$(CC) -c kast.ml
	$(CC) -c runtime.ml
	$(CC) -c iast2kast.ml
	$(CC) -c bc.ml
	$(CC) -c kast2bc.ml
	$(CC) -c bc_print.ml
	$(CC) -c parser.mli
	$(CC) -c lexer.ml
	$(CC) -c parser.ml
	$(CC) -c main.ml
	$(CC) -o $(EXE) parseutils.cmo lexer.cmo types.cmo past.cmo  past_print.cmo ast_print.cmo ast.cmo past2ast.cmo ast_inline.cmo ast_fold.cmo ast_globz.cmo freevr.cmo  ast_lift.cmo  runtime.cmo  iast.cmo ast2iast.cmo iast_tailrec.cmo typing.cmo  kast.cmo iast2kast.cmo bc.cmo kast2bc.cmo bc_print.cmo parser.cmo main.cmo

clean:
	rm -f $(EXE) *.cmo *.cmi compile *.vm *.tst *.out 