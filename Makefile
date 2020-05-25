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
MINIML-STDLIB=stdlib/pervasives.ml stdlib/array.ml stdlib/string.ml # pour mini-ml
VM=vm/
ZAM-STDLIB=$(VM)stdlib/
ZAM-MINIML=$(VM)zam-miniML/
ZAM-OCAML=$(VM)zam-OCaml/
ZAM_INPUT=input.ml
ZAM_SRC=src/mlvalues.ml src/prims.ml \
		src/domain.ml src/block.ml src/data.ml src/gc.ml src/alloc.ml\
		src/call.ml $(ZAM_INPUT) src/interp.ml src/main.ml
ZAM_BIN=$(ZAM-MINIML)bin/

LINK=$(VM)link/
PACK=$(VM)pack/

MKFLAGS=

BENCHS=benchs/

all:	zam-miniML

miniML:
	make -C $(MINIML) $(MKFLAGS)

clean-miniML:
	make clean -C $(MINIML) $(MKFLAGS)

cmo:
	make MLFILES="$(foreach f,$(MLFILES),../../$(f))" -C $(PACK) $(MKFLAGS)

link:
	make MLFILES="$(foreach f,$(MLFILES),../../$(f))" -C $(LINK) $(MKFLAGS)

obytelib:
	dune build $(VM)bytecode/obytelibParser.exe
	./_build/default/$(VM)bytecode/obytelibParser.exe $(PACK)main.cmo >> obytelib.log

zam-miniML:	clean miniML cmo obytelib
	mkdir -p $(ZAM_BIN)
	cd $(MINIML); ./compile $(ASSERT) $(MINIML-FLAGS) $(TYPECHECK) -dst=$(ROOT)$(ZAM_BIN) $(MINIML-STDLIB) $(foreach f,$(ZAM_SRC),$(ROOT)$(ZAM-MINIML)$(f)); cd $(ROOT)

zam-ocaml:	 cmo obytelib
	make -C $(ZAM-OCAML) $(MKFLAGS)

zam-miniML-run:	zam-miniML
	nand2tetris/tools/VMEmulator.sh
	
zam-ocaml-run:	
	make zam-ocaml MKFLAGS=--silent --no-print-directory
	echo ; ./$(ZAM-OCAML)zam.exe ; echo ; echo

clean:	clean-miniML
	rm -rf obytelib.log
	rm -rf $(ZAM_BIN) 
	rm -rf $(ZAM_INPUT)
	make clean-all -C $(LINK)
	make clean-all -C $(ZAM-OCAML)
	rm -rf $(ZAM-MINIML)src/*.cm[oi]
	rm -rf $(ZAM-OCAML)*.cm[oi]
	rm -rf $(ZAM-STDLIB)*.cm[oi]
	rm -rf $(BENCHS)*.cm[oi]


test:
	make zam-ocaml-run MLFILES=$(BENCHS)f91.ml
	make zam-ocaml-run MLFILES=$(BENCHS)syracuse.ml
	make zam-ocaml-run MLFILES=$(BENCHS)fact_tail.ml
	make zam-ocaml-run MLFILES=$(BENCHS)fact.ml
	make zam-ocaml-run MLFILES=$(BENCHS)fib_tail.ml
	make zam-ocaml-run MLFILES=$(BENCHS)fib.ml
	make zam-ocaml-run MLFILES=$(BENCHS)oddeven.ml
	make zam-ocaml-run MLFILES=$(BENCHS)pascal.ml
	make zam-ocaml-run MLFILES=$(BENCHS)rec.ml
	make zam-ocaml-run MLFILES=$(BENCHS)list_iter.ml
	make zam-ocaml-run MLFILES=$(BENCHS)list_rev.ml
	make zam-ocaml-run MLFILES=$(BENCHS)list_append.ml
	make zam-ocaml-run MLFILES=$(BENCHS)list_fold_left.ml
	make zam-ocaml-run MLFILES=$(BENCHS)list_map.ml
	make zam-ocaml-run MLFILES=$(BENCHS)list_map2.ml
	make zam-ocaml-run MLFILES=$(BENCHS)tests/clos/clos0.ml
	make zam-ocaml-run MLFILES=$(BENCHS)tests/clos/clos1.ml
	make zam-ocaml-run MLFILES=$(BENCHS)tests/clos/clos2.ml 
	make zam-ocaml-run MLFILES=$(BENCHS)tests/rec/fact.ml
	make zam-ocaml-run MLFILES=$(BENCHS)tests/rec/fib.ml
	make zam-ocaml-run MLFILES=$(BENCHS)tests/appterm/fact.ml
	make zam-ocaml-run MLFILES=$(BENCHS)tests/appterm/fib.ml
	make zam-ocaml-run MLFILES=$(BENCHS)tests/appterm/ackermann.ml
	make zam-ocaml-run MLFILES=$(BENCHS)tests/grab/grab.ml
	make zam-ocaml-run MLFILES=$(BENCHS)tests/grab/grab2.ml
	make zam-ocaml-run MLFILES="vm/stdlib/array.ml $(BENCHS)tests/array/arr.ml"
	make zam-ocaml-run MLFILES=$(BENCHS)tests/alloc/alloc.ml
	make zam-ocaml-run MLFILES="vm/stdlib/array.ml $(BENCHS)tests/alloc/alloc_array.ml"
	make zam-ocaml-run MLFILES="$(BENCHS)tests/segdata/m.ml $(BENCHS)tests/segdata/sstring.ml"
	make zam-ocaml-run MLFILES="$(BENCHS)tests/modules/m1.ml $(BENCHS)tests/modules/m2.ml $(BENCHS)tests/modules/m3.ml"
	make zam-ocaml-run MLFILES=$(BENCHS)tests/loop/for.ml
	make zam-ocaml-run MLFILES=$(BENCHS)tests/variants/opt.ml
	make zam-ocaml-run MLFILES=$(BENCHS)tests/variants/list_012345.ml
	make zam-ocaml-run MLFILES=$(BENCHS)tests/variants/list0.ml
	make zam-ocaml-run MLFILES=$(BENCHS)tests/variants/list2.ml
	make zam-ocaml-run MLFILES="$(BENCHS)tests/modules/m1.ml $(BENCHS)tests/modules/m2.ml $(BENCHS)tests/modules/m3.ml $(BENCHS)tests/modules/m4.ml"

