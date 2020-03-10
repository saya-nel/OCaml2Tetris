
all:
	ocamlc -c parseutils.ml
	ocamllex lexer.mll       # generates lexer.ml
	ocamlyacc parser.mly     # generates parser.ml and parser.mli
	ocamlc -c ast.ml
	ocamlc -c print_ast.ml
	ocamlc -c kast.ml
	ocamlc -c  ast2kast.ml
	ocamlc -c  bytecode.ml
	ocamlc -c  kast2bytecode.ml
	ocamlc -c bytecode2string.ml
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c utils.ml
	ocamlc -c main.ml
	ocamlc -o compile parseutils.cmo lexer.cmo ast.cmo print_ast.cmo kast.cmo ast2kast.cmo bytecode.cmo kast2bytecode.cmo bytecode2string.cmo parser.cmo utils.cmo main.cmo

clean:
	rm *.cmo *.cmi compile