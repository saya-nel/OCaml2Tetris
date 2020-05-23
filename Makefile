# variante run.sh

# usage :
# make zam-miniML-run MLFILES="f1.ml f2.ml f3.ml" 
# make zam-ocaml-run MLFILES="f1.ml f2.ml f3.ml"

MLFILES= # foo.ml, sources à executer par la zam
DIR=  # relatif !!

I=
ASSERT=-assert
TYPECHECK=-typecheck
ZAM_BIN=zam/bin
OPT=

MINIML=mini-ml/
ZAM_INPUT=zam/input.ml

# chemins relatifs depuis mini-ml
ROOT=../
STDLIB=stdlib/pervasives.ml stdlib/array.ml stdlib/string.ml
ZAM_SRC=zam/src/mlvalues.ml zam/src/prims.ml \
		zam/src/domain.ml zam/src/block.ml zam/src/data.ml zam/src/gc.ml zam/src/alloc.ml\
		zam/src/call.ml $(ZAM_INPUT) zam/src/interp.ml zam/src/main.ml

INTEROP=interop/

all:	zam-miniML

miniML:
	make -C $(MINIML)

clean-miniML:
	make clean -C $(MINIML)

cmo:
	echo $(foreach f,$(MLFILES),$(INTEROP)$(f))
	make MLFILES="$(foreach f,$(MLFILES),../$(f))" -C $(INTEROP)

obytelib:
	dune build bytecode/obytelibParser.exe
	./_build/default/bytecode/obytelibParser.exe $(MLFILES:.ml=.cmo)

zam-miniML:	clean miniML cmo obytelib
	mkdir -p $(ZAM_BIN)
	cd $(MINIML); ./compile $(ASSERT) $(OPT) $(TYPECHECK) -dst=$(ROOT)$(ZAM_BIN) $(STDLIB) $(foreach f,$(ZAM_SRC),$(ROOT)$(f)); cd $(ROOT)

zam-ocaml:	 cmo obytelib
	make -C zam/src/ocaml

zam-miniML-run:	zam-miniML
	nand2tetris/tools/VMEmulator.sh
	

zam-ocaml-run:	zam-ocaml
	./zam/src/ocaml/zam

clean:	clean-miniML
	rm -rf $(ZAM_BIN)
	rm -rf $(ZAM_INPUT)
	make clean -C $(INTEROP)
	make clean -C zam/src/ocaml


