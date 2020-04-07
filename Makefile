# variante run.sh

# usage :
# make INPUT=samples/test.cmo        # prépare le cmo, compile la zam et lance le simulateur
# make short INPUT=samples/test.cmo  # prépare le cmo et compile la zam 


MINIML=mini-ml/
STDLIB=stdlib/pervasives.ml stdlib/array.ml stdlib/string.ml
INPUT=
ZAM_SRC=zam/mlvalues.ml zam/prims.ml zam/check.ml zam/input.ml zam/interp.ml

all: simul

short:	bytecode zam

simul:	bytecode zam
	nand2tetris/tools/VMEmulator.sh

miniML:
	make -C $(MINIML)

zam:	miniML
	cd $(MINIML); ./compile $(STDLIB) $(foreach FILE,$(ZAM_SRC),../$(FILE)); cd ..

bytecode: obytelibParser cmo
	./_build/default/bytecode/obytelibParser.exe $(INPUT)

obytelibParser:
	dune build bytecode/obytelibParser.exe

cmo:
	ocamlc $(INPUT)

clean:
	make clean -C $(MINIML)
