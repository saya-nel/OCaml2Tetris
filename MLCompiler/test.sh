#!/bin/bash

DIR=samples


./MLCompiler.sh $DIR/sample001.ml
./MLCompiler.sh $DIR/sample002.ml
./MLCompiler.sh $DIR/sample003.ml
./MLCompiler.sh $DIR/sample004.ml
./MLCompiler.sh $DIR/sample005.ml
./MLCompiler.sh $DIR/sample006.ml
./MLCompiler.sh $DIR/sample007.ml
./MLCompiler.sh $DIR/sample008.ml
./MLCompiler.sh $DIR/sample009.ml
./MLCompiler.sh $DIR/sample010.ml

./compile $DIR/main.ml
