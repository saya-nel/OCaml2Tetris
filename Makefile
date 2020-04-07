# variante run.sh

# usage :
# make INPUT=samples/test.cmo        # prépare le cmo, compile la zam et lance le simulateur
# make short INPUT=samples/test.cmo  # prépare le cmo et compile la zam 

INPUT=

ZAM_BIN=zam/bin

MINIML=mini-ml/

# chemins relatifs depuis mini-ml
ROOT=../
STDLIB=stdlib/pervasives.ml stdlib/array.ml stdlib/string.ml
ZAM_INPUT=../zam/input.ml
ZAM_SRC=../zam/src/mlvalues.ml ../zam/src/prims.ml ../zam/src/check.ml ../zam/src/interp.ml ../zam/src/main.ml


all: simul

short:	bytecode zam

simul:	bytecode zam
	nand2tetris/tools/VMEmulator.sh

miniML:
	make -C $(MINIML)

zam:	miniML
	cd $(MINIML); ./compile -dst=$(ROOT)$(ZAM_BIN) $(STDLIB) $(ZAM_INPUT) $(ZAM_SRC); cd $(ROOT)

bytecode: obytelibParser cmo
	./_build/default/bytecode/obytelibParser.exe $(INPUT)

obytelibParser:
	dune build bytecode/obytelibParser.exe

cmo:
	ocamlc $(INPUT)
	rm a.out
clean:
	rm -f $(ZAM_BIN)/*
	make clean -C $(MINIML)

