#!/bin/bash

function CheckReturnCode { 
if [ $? -ne 0 ]; then
    echo "echec."
    exit 1
fi
}

./compile -parse $1 
CheckReturnCode

ocamlopt -i $1 
CheckReturnCode

./compile $1
CheckReturnCode

echo
echo "code octet engendré avec succès."