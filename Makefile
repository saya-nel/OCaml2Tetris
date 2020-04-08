# variante run.sh

# usage :
# make I=samples/test.cmo        # prépare le cmo, compile la zam et lance le simulateur
# make short I=samples/test.cmo  # prépare le cmo et compile la zam 

I=

ZAM_BIN=zam/bin

MINIML=mini-ml/
ZAM_INPUT=zam/input.ml

# chemins relatifs depuis mini-ml
ROOT=../
STDLIB=stdlib/pervasives.ml stdlib/array.ml stdlib/string.ml
ZAM_SRC=../zam/src/check.ml ../zam/src/mlvalues.ml ../zam/src/prims.ml ../zam/src/interp.ml ../zam/src/main.ml


all: simul

short:	bytecode zam

simul:	bytecode zam
	nand2tetris/tools/VMEmulator.sh

miniML:
	make -C $(MINIML)

zam:	miniML
	mkdir -p $(ZAM_BIN)
	cd $(MINIML); ./compile -assert -typecheck -dst=$(ROOT)$(ZAM_BIN) $(STDLIB) $(ROOT)$(ZAM_INPUT) $(ZAM_SRC); cd $(ROOT)

bytecode: obytelibParser cmo
	./_build/default/bytecode/obytelibParser.exe $(I)

obytelibParser:
	dune build bytecode/obytelibParser.exe

cmo:
	ocamlc $(I)
	rm -f a.out
clean:
	rm -Rf $(ZAM_BIN)
	rm -rf $(ZAM_INPUT)
	make clean -C $(MINIML)


