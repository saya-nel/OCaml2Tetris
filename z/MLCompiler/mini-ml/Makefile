
all:
	ocamlc -c parseutils.ml
	ocamllex lexer.mll       # generates lexer.ml
	ocamlyacc parser.mly     # generates parser.ml and parser.mli
	ocamlc -c utils.ml
	ocamlc -c ast.ml
	ocamlc -c kast.ml
	ocamlc -c bytecode.ml
	ocamlc -c compiler.ml
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c main.ml
	ocamlc -o compile parseutils.cmo lexer.cmo utils.cmo ast.cmo kast.cmo bytecode.cmo compiler.cmo parser.cmo main.cmo

clean:
	rm *.cmo *.cmi compile
