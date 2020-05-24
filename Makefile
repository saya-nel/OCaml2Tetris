# variante run.sh

# usage :
# make zam-miniML-run MLFILES="f1.ml f2.ml f3.ml" 
# make zam-ocaml-run MLFILES="f1.ml f2.ml f3.ml"

MLFILES= # sources à executer par la zam

ASSERT=-assert
TYPECHECK=-typecheck
MINIML-FLAGS=

# chemins relatifs depuis mini-ml
MINIML=mini-ml/
ROOT=../
STDLIB=stdlib/pervasives.ml stdlib/array.ml stdlib/string.ml
VM=vm/
ZAM-MINIML=$(VM)zam-miniML/
ZAM-OCAML=$(VM)zam-OCaml/
ZAM_INPUT=input.ml
ZAM_SRC=src/mlvalues.ml src/prims.ml \
		src/domain.ml src/block.ml src/data.ml src/gc.ml src/alloc.ml\
		src/call.ml $(ZAM_INPUT) src/interp.ml src/main.ml
ZAM_BIN=$(ZAM-MINIML)bin/

LINK=$(VM)link/

MKFLAGS=

all:	zam-miniML

miniML:
	make -C $(MINIML) $(MKFLAGS)

clean-miniML:
	make clean -C $(MINIML) $(MKFLAGS)

cmo:
	# echo $(foreach f,$(MLFILES),$(LINK)$(f))
	make MLFILES="$(foreach f,$(MLFILES),../../$(f))" -C $(LINK) $(MKFLAGS)

obytelib:
	dune build $(VM)bytecode/obytelibParser.exe
	./_build/default/$(VM)bytecode/obytelibParser.exe $(MLFILES:.ml=.cmo) >> obytelib.log

zam-miniML:	clean miniML cmo obytelib
	mkdir -p $(ZAM_BIN)
	cd $(MINIML); ./compile $(ASSERT) $(MINIML-FLAGS) $(TYPECHECK) -dst=$(ROOT)$(ZAM_BIN) $(STDLIB) $(foreach f,$(ZAM_SRC),$(ROOT)$(ZAM-MINIML)$(f)); cd $(ROOT)

zam-ocaml:	 cmo obytelib
	make -C $(ZAM-OCAML) $(MKFLAGS)

zam-miniML-run:	zam-miniML
	nand2tetris/tools/VMEmulator.sh
	
zam-ocaml-run:	
	make zam-ocaml MKFLAGS=--silent --no-print-directory
	echo ; ./$(ZAM-OCAML)zam.exe ; echo ; echo

clean:	clean-miniML
	rm -rf $(ZAM_BIN)
	rm -rf $(ZAM_INPUT)
	make clean-all -C $(LINK)
	make clean-all -C $(ZAM-OCAML)

test:
	make zam-ocaml-run MLFILES=tests/clos/clos0.ml
	make zam-ocaml-run MLFILES=tests/clos/clos1.ml
	make zam-ocaml-run MLFILES=tests/clos/clos2.ml 
	make zam-ocaml-run MLFILES=tests/rec/fact.ml
	make zam-ocaml-run MLFILES=tests/rec/fib.ml
	make zam-ocaml-run MLFILES=tests/appterm/fact.ml
	make zam-ocaml-run MLFILES=tests/appterm/ackermann.ml
	make zam-ocaml-run MLFILES=tests/grab/grab.ml
	make zam-ocaml-run MLFILES=tests/grab/grab2.ml
	make zam-ocaml-run MLFILES=tests/grab/f91.ml
	make zam-ocaml-run MLFILES=tests/array/arr.ml
	make zam-ocaml-run MLFILES=tests/alloc/alloc.ml
	make zam-ocaml-run MLFILES=tests/alloc/alloc_array.ml
	make zam-ocaml-run MLFILES="tests/segdata/m.ml tests/segdata/sstring.ml"
	make zam-ocaml-run MLFILES="tests/modules/m1.ml tests/modules/m2.ml tests/modules/m3.ml"
	make zam-ocaml-run MLFILES=tests/loop/for.ml
	make zam-ocaml-run MLFILES=tests/variants/opt.ml
	make zam-ocaml-run MLFILES=tests/variants/list_012345.ml
	make zam-ocaml-run MLFILES=tests/variants/list.ml
	make zam-ocaml-run MLFILES=tests/variants/list2.ml
	make zam-ocaml-run MLFILES=tests/variants/list_iter.ml
	make zam-ocaml-run MLFILES=tests/variants/list_append.ml

